module Network.BitTorrent.Client
    ( Client(..)
    , TrackerState(..)
    , PeerState
    , PeerStateSnapshot(..)
    , peerStateToSnapshot
    , createClient
    , startDownload
    ) where

import Prelude hiding (zipWith, or)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import Control.Monad
import Control.Monad.ST (runST)

import Data.Array.BitArray
import Data.Array.BitArray.ByteString (fromByteString)
import Data.Array.BitArray.ST (thaw, unsafeFreeze, writeArray)
import Data.Binary (Word32, decode)
import Data.Maybe (listToMaybe)
import qualified Data.STM.LinkedList as List
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP (simpleHTTP, getRequest, rspBody)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)

import System.IO (withBinaryFile, hSeek, IOMode(..), SeekMode(..))
import System.IO.Error (tryIOError)

import Data.Torrent
import Network.BitTorrent.Peer
import Network.BitTorrent.Tracker

data Client = Client
    { clientId :: BS.ByteString
    , activePeers :: TVar Int
    }

data Download = Download
    { dTorrent :: Torrent
    , dTracker :: TrackerState
    , dPeers :: List.LinkedList PeerState
    , dPiecesPresent :: TVar PiecesPresentArray
    , dActivePieces :: List.LinkedList [(Word32, TVar BlockState)]
    -- ^ list of pieces currently downloading.
    --   after download of piece is complete,
    --   its hash is checked, piece is written to disk
    --   and removed from list
    }

data BlockState = BlockNotRequested Message
                | BlockRequested Message [PeerState]
                | BlockCompleted BS.ByteString

data TrackerState = TrackerState
    { tsTracker :: BS.ByteString
    , tsTrackerResponse :: TVar TrackerResponse
    , tsUpdateDelay :: TVar (Maybe Delay)
    }

data PeerState = PeerState
    { psPeer :: Peer
    , psSocket :: Socket
    , psInterested :: TVar Bool
    , psUnchocked :: TVar Bool
    , psWeInterested :: TVar Bool
    , psWeUnchocked :: TVar Bool
    , psPieces :: TVar PiecesPresentArray
    , psDownload :: Download
    , psEnqueuedNumber :: TVar Int
    }

data PeerStateSnapshot = PeerStateSnapshot
    { pssPeer :: Peer
    , pssInterested :: Bool
    , pssUnchocked :: Bool
    , pssWeInterested :: Bool
    , pssWeUnchocked :: Bool
    , pssPieces :: PiecesPresentArray
    }

peerStateToSnapshot :: PeerState -> STM PeerStateSnapshot
peerStateToSnapshot ps =
    PeerStateSnapshot (psPeer ps)
        <$> readTVar (psInterested ps)
        <*> readTVar (psUnchocked ps)
        <*> readTVar (psWeInterested ps)
        <*> readTVar (psWeUnchocked ps)
        <*> readTVar (psPieces ps)

type PiecesPresentArray = BitArray Word32

createClient :: BS.ByteString -> IO Client
createClient peerId = Client peerId <$> newTVarIO 0

bitArrayForTorrent :: Torrent -> BitArray Word32
bitArrayForTorrent t = array (0, numPieces t - 1) []

bitArrayFromString :: Torrent -> BS.ByteString -> BitArray Word32
bitArrayFromString t str = fromByteString (0, numPieces t - 1) str

newPeerState :: Download -> Peer -> Socket -> IO PeerState
newPeerState download peer socket =
    PeerState peer socket
        <$> newTVarIO False
        <*> newTVarIO False
        <*> newTVarIO False
        <*> newTVarIO False
        <*> newTVarIO (bitArrayForTorrent $ dTorrent download)
        <*> pure download
        <*> newTVarIO 0

announceToURI :: Client -> Torrent -> BS.ByteString -> IO TrackerResponse
announceToURI client torrent announce = do
    let uri = BS.unpack $ announce `BS.append` announceParams torrent (clientId client)
    resp <- tryIOError (simpleHTTP $ getRequest uri)
    case resp of
        Left err -> return . Failure . BS.pack $ show err
        Right res -> return $ either (Failure . BS.pack) id $
            case res of
                Left err -> Left $ show err
                Right rsp -> decodeResponse . BS.pack $ rspBody rsp

pollTracker :: Client -> Torrent -> BS.ByteString -> IO TrackerState
pollTracker client torrent announce = do
    state <- TrackerState announce
        <$> newTVarIO (Failure $ BS.pack "Not requested")
        <*> (newTVarIO . Just =<< newDelay 0)
    forkIO $ updateTracker client torrent state
    return state

updateTracker :: Client -> Torrent -> TrackerState -> IO ()
updateTracker client torrent (TrackerState tracker responseVar delayVar) = forever $ do
        atomically waitUpdate
        response <- announceToURI client torrent tracker
        d <- newDelay $ 1000000 * case response of
                Failure _ -> 20
                Response interval _ -> fromIntegral interval
        atomically $ do
            writeTVar delayVar (Just d)
            writeTVar responseVar response
    where
        waitUpdate = do
            mDelay <- readTVar delayVar
            case mDelay of
                Nothing -> retry
                Just delay -> waitDelay delay
            writeTVar delayVar Nothing

startDownload :: Client -> Torrent -> IO Download
startDownload client torrent = do
    download <- Download torrent
             <$> pollTracker client torrent (tAnnounce torrent)
             <*> List.emptyIO
             <*> newTVarIO (bitArrayForTorrent torrent)
             <*> List.emptyIO
    forkIO $ downloadBackground client download
    return download

downloadBackground :: Client -> Download -> IO ()
downloadBackground client download = do
    peers <- atomically $ waitForPeers download
    forM_ peers $ \peer ->
        forkIO $ handlePeer client download peer

handlePeer :: Client -> Download -> Peer -> IO ()
handlePeer client download peer = do
    putStrLn $ "Connecting to " ++ show peer
    conn <- connectToPeer peer
    case conn of
        Left err -> putStrLn $ "Error: " ++ err
        Right sock -> do
            sendHandshake (clientId client) (getInfoHash $ dTorrent download) sock
            response <- decode . BL.fromStrict <$> recv sock 68 :: IO Handshake
            putStr "Got response: "
            print response
            -- TODO: Check handshake
            peerState <- newPeerState download peer sock
            node <- atomically $ do
                incTVar $ activePeers client
                List.append peerState (dPeers download)
            mapM_ (handleMessage peerState) =<< socketToMessages sock
            atomically $ do
                decTVar $ activePeers client
                List.delete node
    where
        incTVar var = do
            val <- readTVar var
            writeTVar var (val + 1)
            return $ val + 1

        decTVar var = do
            val <- readTVar var
            writeTVar var (val - 1)
            return $ val - 1


handleMessage :: PeerState -> Either String Message -> IO ()
handleMessage _ (Left str) = putStrLn $ "Error: " ++ str
handleMessage peerState (Right msg) = do
    case msg of
        Bitfield str -> do
            atomically $
                writeTVar (psPieces peerState) (bitArrayFromString (dTorrent $ psDownload peerState) str)
            checkInterested peerState
        Have i -> do
            atomically $ do
                bitarray <- readTVar $ psPieces peerState
                new <- return $ runST $ do
                    arr <- thaw bitarray
                    writeArray arr i True
                    unsafeFreeze arr
                writeTVar (psPieces peerState) new
            checkInterested peerState
        Choke -> atomically $ writeTVar (psWeUnchocked peerState) False
        Unchoke -> do
            atomically $ writeTVar (psWeUnchocked peerState) True
            enqueueBlocks peerState
        Interested -> atomically $ writeTVar (psInterested peerState) True
        NotInterested -> atomically $ writeTVar (psInterested peerState) False
        Piece idx begin block -> do
            putStr "Got block: "
            print (idx, begin)
            act <- atomically $ do
                modifyTVar (psEnqueuedNumber peerState) (subtract 1)
                saveBlock (psDownload peerState) idx begin block
            act
            enqueueBlocks peerState
        _ -> putStr "Unhandled: " >> print msg

saveBlock :: Download -> Word32 -> Word32 -> BS.ByteString -> STM (IO ())
saveBlock download idx begin block = do
    mblock <- findBlock (dActivePieces download) idx begin
    case mblock of
        Nothing -> return $ do
            putStr "Error: Can't find downloaded block: "
            print (idx, begin)
        Just (node, var) -> do
            writeTVar var (BlockCompleted block)
            let wholeList = List.value node
            isCompl <- isCompletedPiece wholeList
            when isCompl $ do
                List.delete node
                modifyTVar' (dPiecesPresent download) (// [(idx, True)])
            return $ if isCompl then writePieceToFile download idx wholeList else return ()

writePieceToFile :: Download -> Word32 -> [(Word32, TVar BlockState)] -> IO ()
writePieceToFile download idx lst = do
    putStr "Writing piece to file: "
    print idx
    allChunksStates <- forM lst $ \(_, var) -> readTVarIO var
    let piece = BS.concat $ fmap (\(BlockCompleted str) -> str) allChunksStates
    let pieceSize = idPieceLength . tInfoDict . dTorrent $ download
    withBinaryFile (BS.unpack . idName . tInfoDict . dTorrent $ download) ReadWriteMode $ \h -> do
        hSeek h AbsoluteSeek (fromIntegral idx * pieceSize)
        BS.hPut h piece

isCompletedPiece :: [(Word32, TVar BlockState)] -> STM Bool
isCompletedPiece lst = all isCompleted <$> (forM lst $ \(_, var) -> readTVar var)
    where
        isCompleted (BlockCompleted _) = True
        isCompleted _ = False

-- TODO: Refactor this
findBlock :: List.LinkedList [(Word32, TVar BlockState)] -> Word32 -> Word32 -> STM (Maybe (List.Node [(Word32, TVar BlockState)], TVar BlockState))
findBlock list idx begin = do
    mnode <- List.start list
    recurseFindBlock mnode idx begin

recurseFindBlock :: Maybe (List.Node [(Word32, TVar BlockState)]) -> Word32 -> Word32 -> STM (Maybe (List.Node [(Word32, TVar BlockState)], TVar BlockState))
recurseFindBlock Nothing _ _ = return Nothing
recurseFindBlock (Just node) idx begin = do
    el <- listToMaybe <$> filterM blockInList (List.value node)
    case el of
        Nothing -> do
            n <- List.next node
            recurseFindBlock n idx begin
        Just (_,e) -> return $ Just (node, e)
    where
        blockInList (_, blockStateVar) = do
            blockState <- readTVar blockStateVar
            case blockState of
                BlockNotRequested (Request midx mbegin _) -> return $ idx == midx && begin == mbegin
                BlockRequested (Request midx mbegin _) _ -> return $ idx == midx && begin == mbegin
                _ -> return False

enqueueBlocks :: PeerState -> IO ()
enqueueBlocks peerState = do
    block <- atomically $ do
        n <- readTVar $ psEnqueuedNumber peerState
        result <- if n > 10
            then return []
            else do
                blocks <- pickBlock peerState
                forM blocks $ \block -> do
                    BlockNotRequested msg <- readTVar block
                    writeTVar block $ BlockRequested msg [peerState]
                    return msg
        modifyTVar' (psEnqueuedNumber peerState) (+ length result)
        return result
    forM_ block $ \x -> do
        putStr "Requesting block: "
        print x
        sendMessage x (psSocket peerState)

-- | Picks next blocks to request from client
pickBlock :: PeerState -> STM [TVar BlockState]
pickBlock ps = do
    unfinished <- getUnenqueued (psDownload ps)
    case unfinished of
        Left [] -> return []
        Left lst -> do
            xs <- filterM (ps `hasPiece`) lst
            case xs of
                [] -> return []
                (x:_) -> enqueuePiece (psDownload ps) x
        Right msgs -> return msgs

-- | Checks whether peer has given piece
hasPiece :: PeerState -> Word32 -> STM Bool
hasPiece PeerState{ psPieces = piecesVar } piece = do
    piecesPresent <- readTVar piecesVar
    return $ piecesPresent ! piece

enqueuePiece :: Download -> Word32 -> STM [TVar BlockState]
enqueuePiece Download{ dTorrent = torrent, dActivePieces = activePieces } idx = do
    let msgs = pieceToMessages torrent idx
    pieceInfo <- forM msgs $ \msg -> do
        tvar <- newTVar $ BlockNotRequested msg
        return (idx, tvar)
    List.append pieceInfo activePieces
    return $ fmap snd pieceInfo

getUnenqueued :: Download -> STM (Either [Word32] [TVar BlockState] )
getUnenqueued download = do
        unfinished <- filterActiveBlocks notRequested download
        if Prelude.null unfinished
            then do
                pieces <- getUnenqueuedPieces download
                return $ Left pieces
            else return . Right . fmap snd $ unfinished
    where
        notRequested (_,BlockNotRequested _) = True
        notRequested _ = False

getUnenqueuedPieces :: Download -> STM [Word32]
getUnenqueuedPieces Download{ dPiecesPresent = piecesPresentVar } = do
    piecesPresent <- readTVar piecesPresentVar
    return . fmap fst . filter (not.snd) $ assocs piecesPresent

filterActiveBlocks :: ((Word32, BlockState) -> Bool) -> Download -> STM [(Word32, TVar BlockState)]
filterActiveBlocks f Download{ dActivePieces = activePieces } = do
    pieces <- List.toList activePieces
    unpackedPacked <- sequence $ do
        pieceBlock <- pieces
        original <- pieceBlock
        return $ do
            let (piece, blockStateVar) = original
            blockState <- readTVar blockStateVar
            return ((piece, blockState), original)
    return . fmap snd $ filter (f . fst) unpackedPacked

pieceToMessages :: Torrent -> Word32 -> [Message]
pieceToMessages t idx = fmap blockToMessage [0..nBlocks-1]
    where
        nBlocks = (sizeOfPiece + len - 1) `div` len
        totalLen = totalLength $ tInfoDict t
        pieceLength = idPieceLength $ tInfoDict t
        sizeOfPiece = min (totalLen - pieceLength * fromIntegral idx) pieceLength
        len = 16384
        blockToMessage block = Request idx (fromIntegral block * fromIntegral len) blockLen
            where blockLen = fromIntegral $ min (sizeOfPiece - block * len) len

checkInterested :: PeerState -> IO ()
checkInterested peerState = do
    should <- atomically $ do
        weInterested <- readTVar $ psWeInterested peerState
        if weInterested
            then return False
            else do
                hasPieces <- hasNeededPieces peerState
                writeTVar (psWeInterested peerState) hasPieces
                return hasPieces
    when should $ sendMessage Interested (psSocket peerState)

hasNeededPieces :: PeerState -> STM Bool
hasNeededPieces ps = do
    peerPieces <- readTVar $ psPieces ps
    ourPieces <- readTVar . dPiecesPresent $ psDownload ps
    return $ or $ zipWith (\x y -> y && not x) ourPieces peerPieces

waitForPeers :: Download -> STM [Peer]
waitForPeers download = do
    response <- readTVar $ tsTrackerResponse $ dTracker download
    case response of
        Response _ peers | not $ Prelude.null peers -> return peers
        _ -> retry
