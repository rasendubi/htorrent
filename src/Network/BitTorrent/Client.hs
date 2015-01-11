module Network.BitTorrent.Client
where

import System.IO.Error
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import Control.Monad.STM
import Control.Monad
import qualified Data.ByteString.Char8 as BS

import Data.BitArray
import Data.STM.LinkedList

import Data.Torrent
import Network.BitTorrent.Tracker

import Network.HTTP hiding (Response, Request)
-- import Network.Socket.ByteString

data Client = Client
    { clientId :: BS.ByteString
    -- , clientDownload :: TVar Download
    }

data Download = Download
    { dTorrent :: Torrent
    , dTracker :: TrackerState
    , dPeers :: LinkedList PeerState
    }

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
    , psInterested :: TVar Bool
    , psUnchocked :: TVar Bool
    , psWeInterested :: TVar Bool
    , psWeUnchocked :: TVar Bool
    , psPieces :: TVar BitArray
    }

bitArrayForTorrent :: Torrent -> BitArray
bitArrayForTorrent t = bitArray (0, numPieces t) []

newPeerState :: Torrent -> Peer -> IO PeerState
newPeerState t peer =
    PeerState peer <$> newTVarIO False
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
    forkIO $ downloadBackground client download
    return download

downloadBackground :: Client -> Download -> IO ()
downloadBackground client download = do
    peers <- atomically $ waitForPeers download
    print peers

waitForPeers :: Download -> STM [Peer]
waitForPeers download = do
    let responseVar = tsTrackerResponse $ dTracker download
    response <- readTVar responseVar
    case response of
        Response _ peers | not $ Prelude.null peers -> return peers
        _ -> retry
