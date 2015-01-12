module Network.BitTorrent.Client
where

import Prelude hiding (zipWith, or)

import System.IO
import System.IO.Error
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Array.BitArray
import Data.Array.BitArray.ST (thaw, unsafeFreeze, writeArray)
import Data.Array.BitArray.ByteString (fromByteString)
import Data.Binary
import Data.STM.LinkedList
import Data.Maybe (listToMaybe)

import Data.Torrent
import Network.BitTorrent.Tracker
import Network.BitTorrent.Peer

import Network.HTTP hiding (Response, Request)
import Network.Socket (Socket)
import Network.Socket.ByteString

data Client = Client
    { clientId :: BS.ByteString
    -- , clientDownload :: TVar Download
    , activePeers :: TVar Int
    }

createClient :: BS.ByteString -> IO Client
createClient peerId = Client peerId <$> newTVarIO 0

data Download = Download
    { dTorrent :: Torrent
    , dTracker :: TrackerState
    , dPeers :: LinkedList PeerState
    , dPiecesPresent :: TVar PiecesPresentArray
    , dActivePieces :: LinkedList [(Word32, TVar BlockState)]
    -- ^ list of pieces currently downloading.
    --   after download of piece is complete,
    --   its hash is checked and piece is written to disk
    --   and removed from list
    }

data BlockState = BlockNotRequested Message
                | BlockRequested Message [PeerState]
                | BlockCompleted BS.ByteString

data TrackerState = TrackerState
    { tsTracker :: BS.ByteString
    , tsTrackerResponse :: TVar TrackerResponse
    , tsUpdateDelay :: TrackerUpdateState
    }

type TrackerUpdateState = TVar (Maybe Delay)

updateWait :: TrackerUpdateState -> STM ()
updateWait st = do
    mDelay <- readTVar st
    case mDelay of
        Nothing -> retry
        Just delay -> waitDelay delay
    writeTVar st Nothing

data PeerState = PeerState
    { psPeer :: Peer
    , psSocket :: Socket
    , psInterested :: TVar Bool
    , psUnchocked :: TVar Bool
    , psWeInterested :: TVar Bool
    , psWeUnchocked :: TVar Bool
    , psPieces :: TVar PiecesPresentArray
    }

type PiecesPresentArray = BitArray Word32

bitArrayForTorrent :: Torrent -> BitArray Word32
bitArrayForTorrent t = array (0, numPieces t) []

bitArrayFromString :: Torrent -> BS.ByteString -> BitArray Word32
bitArrayFromString t str = fromByteString (0, numPieces t) str

newPeerState :: Torrent -> Peer -> Socket -> IO PeerState
newPeerState t peer socket =
    PeerState peer socket
        <$> newTVarIO False
        <*> newTVarIO False
        <*> newTVarIO False
        <*> newTVarIO False
        <*> newTVarIO (bitArrayForTorrent t)

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
updateTracker client torrent (TrackerState tracker responseVar delay) = forever $ do
    atomically $ updateWait delay
    response <- announceToURI client torrent tracker
    atomically $ writeTVar responseVar response
    let delayInterval = 1000000 * case response of
            Failure _ -> 20
            Response interval _ -> interval
    d <- newDelay $ fromIntegral delayInterval
    atomically $ writeTVar delay (Just d)

startDownload :: Client -> Torrent -> IO Download
startDownload client torrent = do
    download <- Download torrent
             <$> pollTracker client torrent (tAnnounce torrent)
             <*> emptyIO
             <*> newTVarIO (bitArrayForTorrent torrent)
             <*> emptyIO
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
    conn_ <- connectToPeer peer
    case conn_ of
        Left err -> putStrLn $ "Error: " ++ err
        Right conn -> do
            sendHandshake (clientId client) (getInfoHash $ dTorrent download) conn
            response <- decode . BL.fromStrict <$> recv conn 68 :: IO Handshake
            putStr "Got response: "
            print response
            -- TODO: Check handshake
            peerState <- newPeerState (dTorrent download) peer conn
            node <- atomically $ do
                incTVar $ activePeers client
                append peerState (dPeers download)
            mapM_ (handleMessage download peerState) =<< socketToMessages conn
            atomically $ do
                decTVar $ activePeers client
                delete node
            return ()
                
handleMessage :: Download -> PeerState -> Either String Message -> IO ()
handleMessage _ _ (Left str) = putStrLn $ "Error: " ++ str
handleMessage download peerState (Right msg) = do
    print msg
    case msg of
        Bitfield str -> do
            atomically $
                writeTVar (psPieces peerState) (bitArrayFromString (dTorrent download) str)
            checkInterested download peerState
        Have i -> do
            atomically $ do
                bitarray <- readTVar $ psPieces peerState
                new <- return $ runST $ do
                    arr <- thaw bitarray
                    writeArray arr i True
                    unsafeFreeze arr
                writeTVar (psPieces peerState) new
            checkInterested download peerState
        Choke -> atomically $ writeTVar (psWeUnchocked peerState) False
        Unchoke -> do
            atomically $ writeTVar (psWeUnchocked peerState) True
            enqueueBlocks download peerState
        Interested -> atomically $ writeTVar (psInterested peerState) True
        NotInterested -> atomically $ writeTVar (psInterested peerState) False
        Piece idx begin block -> do
            act <- atomically $ addBlock download idx begin block
            act
        _ -> return ()

addBlock :: Download -> Word32 -> Word32 -> BS.ByteString -> STM (IO ())
addBlock download idx begin block = do
    mblock <- findBlock (dActivePieces download) idx begin
    case mblock of
        Nothing -> return $ do
            putStr "Error: Can't find downloaded block: "
            print (idx, begin)
        Just (node, var) -> do
            writeTVar var (BlockCompleted block)
            let wholeList = value node
            isCompl <- isCompletedPiece wholeList
            when isCompl $ do
                delete node
                modifyTVar' (dPiecesPresent download) (// [(idx, True)])
            return $ if isCompl then writeToDisk download idx wholeList else return ()

writeToDisk :: Download -> Word32 -> [(Word32, TVar BlockState)] -> IO ()
writeToDisk download idx lst = do
    putStrLn "Writing block to disk"
    allChunksStates <- forM lst $ \(_, var) -> readTVarIO var
    let piece = BS.concat $ fmap (\(BlockCompleted str) -> str) allChunksStates
    let pieceSize = idPieceLength . tInfoDict . dTorrent $ download
    withBinaryFile (BS.unpack . idName . tInfoDict . dTorrent $ download) ReadWriteMode $ \h -> do
        hSeek h AbsoluteSeek (fromIntegral idx * pieceSize)
        BS.hPut h piece

isCompletedPiece :: [(Word32, TVar BlockState)] -> STM Bool
isCompletedPiece lst = all isCompleted <$> (forM lst $ \(_, var) -> readTVar var)

isCompleted :: BlockState -> Bool
isCompleted (BlockCompleted _) = True
isCompleted _ = False

findBlock :: LinkedList [(Word32, TVar BlockState)] -> Word32 -> Word32 -> STM (Maybe (Node [(Word32, TVar BlockState)], TVar BlockState))
findBlock list idx begin = do
    mnode <- start list
    recurseFindBlock mnode idx begin

recurseFindBlock :: Maybe (Node [(Word32, TVar BlockState)]) -> Word32 -> Word32 -> STM (Maybe (Node [(Word32, TVar BlockState)], TVar BlockState))
recurseFindBlock Nothing _ _ = return Nothing
recurseFindBlock (Just node) idx begin = do
    el <- listToMaybe <$> filterM blockInList (value node)
    case el of
        Nothing -> do
            n <- next node
            recurseFindBlock n idx begin
        Just (_,e) -> return $ Just (node, e)
    where
        blockInList (_, blockStateVar) = do
            blockState <- readTVar blockStateVar
            case blockState of
                BlockNotRequested (Request midx mbegin _) -> return $ idx == midx && begin == mbegin
                BlockRequested (Request midx mbegin _) _ -> return $ idx == midx && begin == mbegin
                _ -> return False

enqueueBlocks :: Download -> PeerState -> IO ()
enqueueBlocks download peerState = do
    block <- atomically $ do
        blocks <- pickBlock download peerState
        forM blocks $ \block -> do
            BlockNotRequested msg <- readTVar block
            writeTVar block (BlockRequested msg [peerState])
            return msg
    forM_ block $ \x -> sendMessage x (psSocket peerState)

pickBlock :: Download -> PeerState -> STM [TVar BlockState]
pickBlock download ps = do
    -- ourPieces <- readTVar $ dPiecesPresent download
    -- peerPieces <- readTVar $ psPieces ps
    -- let interestingPieces = zipWith (\x y -> y && not x) ourPieces peerPieces
    unfinished <- getUnfinishedBlocks download
    case unfinished of
        Left [] -> return []
        Left lst -> do
            xs <- filterM (ps `hasPiece`) lst
            case xs of
                [] -> return []
                (x:_) -> enqueuePiece download x
        Right msgs -> return msgs

hasPiece :: PeerState -> Word32 -> STM Bool
hasPiece PeerState{ psPieces = piecesVar } piece = do
    piecesPresent <- readTVar piecesVar
    return $ piecesPresent ! piece

enqueuePiece :: Download -> Word32 -> STM [TVar BlockState]
enqueuePiece Download{ dTorrent = torrent, dActivePieces = activePieces } idx = do
    let msgs = pieceToMessages torrent idx
    node <- forM msgs $ \msg -> do
        tvar <- newTVar $ BlockNotRequested msg
        return (idx, tvar)
    append node activePieces
    return $ fmap snd node
        
getUnfinishedBlocks :: Download -> STM (Either [Word32] [TVar BlockState] )
getUnfinishedBlocks download = do
    unfinished <- filterActiveBlocks notRequested download
    if Prelude.null unfinished
        then do
            pieces <- getUnfinishedPieces download
            return $ Left pieces
        else return . Right . fmap snd $ unfinished

getUnfinishedPieces :: Download -> STM [Word32]
getUnfinishedPieces Download{ dPiecesPresent = piecesPresentVar } = do
    piecesPresent <- readTVar piecesPresentVar
    return . fmap fst . filter (not.snd) $ assocs piecesPresent

notRequested :: (Word32, BlockState) -> Bool
notRequested (_,BlockNotRequested _) = True
notRequested _ = False

filterActiveBlocks :: ((Word32, BlockState) -> Bool) -> Download -> STM [(Word32, TVar BlockState)]
filterActiveBlocks f Download{ dActivePieces = activePieces } = do
    pieces <- toList activePieces
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

checkInterested :: Download -> PeerState -> IO ()
checkInterested download peerState = do
    should <- atomically $ do
        weInterested <- readTVar $ psWeInterested peerState
        if weInterested
            then return False
            else do
                sbi <- shouldBeInterested download peerState
                writeTVar (psWeInterested peerState) sbi
                return sbi
    when should $ sendMessage Interested (psSocket peerState)

shouldBeInterested :: Download -> PeerState -> STM Bool
shouldBeInterested download ps = do
    peerPieces <- readTVar $ psPieces ps
    ourPieces <- readTVar $ dPiecesPresent download
    return $ or $ zipWith (\x y -> y && not x) ourPieces peerPieces

incTVar :: (Num a) => TVar a -> STM a
incTVar var = do
    val <- readTVar var
    writeTVar var (val + 1)
    return $ val + 1

decTVar :: (Num a) => TVar a -> STM a
decTVar var = do
    val <- readTVar var
    writeTVar var (val - 1)
    return $ val - 1

waitForPeers :: Download -> STM [Peer]
waitForPeers download = do
    let responseVar = tsTrackerResponse $ dTracker download
    response <- readTVar responseVar
    case response of
        Response _ peers | not $ Prelude.null peers -> return peers
        _ -> retry
