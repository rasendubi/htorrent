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
import Data.BitArray.ST

import Data.Torrent
import Network.BitTorrent.Tracker

import Network.HTTP hiding (Response, Request)
-- import Network.Socket.ByteString

data Client = Client
    { clientId :: BS.ByteString
    , clientDownloads :: [TVar Download]
    }

data Download = Download
    { dTorrent :: Torrent
    , dTrackers :: TVar TrackerState
    , dPeers :: TVar PeerState
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
    , psInterested :: Bool
    , psChocked :: Bool
    , psWeInterested :: Bool
    , psWeUnchocked :: Bool
    , psPieces :: BitArray
    }


newPeerState :: Torrent -> Peer -> PeerState
newPeerState t peer = PeerState peer False False False False (bitArray (0, numPieces t) [])

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
