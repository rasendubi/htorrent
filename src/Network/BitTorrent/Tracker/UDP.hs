{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.BitTorrent.Tracker.UDP
    ( updater
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Control.Monad.State as S
import Control.Monad.State (liftIO)
import Control.Exception
import Control.Concurrent.STM

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Text.URI

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Data.Torrent
import Network.BitTorrent.Tracker.Types

import System.Log.Logger

data UdpTrackerRequest
    = ConnectRequest
        { cqTransactionId :: !Word32
        }
    | AnnounceRequest
        { aqConnectionId :: !Word64
        , aqTransactionId :: !Word32
        , aqInfoHash :: !BS.ByteString
        , aqPeerId :: !BS.ByteString
        , aqDownloaded :: !Word64
        , aqLeft :: !Word64
        , aqUploaded :: !Word64
        , aqEvent :: !Event
        , aqIpAddress :: !Word32
        , aqKey :: !Word32
        , aqNumWant :: !Word32
        , aqPort :: !Word16
        }
    | ScrapeRequest
        { sqConnectionId :: !Word64
        , sqTransactionId :: !Word32
        , sqInfoHash :: [BS.ByteString]
        }
    deriving (Show)

data UdpTrackerResponse
    = ConnectResponse
        { tpTransactionId :: !Word32
        , cpConnectionId :: !Word64
        }
    | AnnounceResponse
        { tpTransactionId :: !Word32
        , apInterval :: !Word32
        , apLeechers :: !Word32
        , apSeeders :: !Word32
        , apPeers :: [Peer]
        }
    | ScrapeResponse
        { tpTransactionId :: !Word32
        , spScrapes :: [Scrape]
        }
    | ErrorResponse
        { tpTransactionId :: !Word32
        , epMessage :: BS.ByteString
        }
    deriving (Show)

data Scrape = Scrape { sSeeders :: !Word32
                     , sCompleted :: !Word32
                     , sLeechers :: !Word32
                     }
    deriving (Show)

data Event = None | Completed | Started | Stopped
    deriving (Show)

instance Binary UdpTrackerRequest where
    put ConnectRequest{..} = do
        putWord64be 0x41727101980
        putWord32be 0
        putWord32be cqTransactionId
    put AnnounceRequest{..} = do
        putWord64be aqConnectionId
        putWord32be 1
        putWord32be aqTransactionId
        putByteString aqInfoHash
        putByteString aqPeerId
        putWord64be aqDownloaded
        putWord64be aqLeft
        putWord64be aqUploaded
        put aqEvent
        putWord32be aqIpAddress
        putWord32be aqKey
        putWord32be aqNumWant
        putWord16be aqPort
    put ScrapeRequest{..} = do
        putWord64be sqConnectionId
        putWord32be 2
        putWord32be sqTransactionId
        putManyWith putByteString sqInfoHash

    get = undefined

instance Binary UdpTrackerResponse where
    get = do
        action <- getWord32be
        case action of
            0 -> ConnectResponse <$> label "transaction_id" getWord32be
                                 <*> label "connection_id"  getWord64be
            1 -> AnnounceResponse <$> label "transaction_id" getWord32be
                                  <*> label "interval"       getWord32be
                                  <*> label "leechers"       getWord32be
                                  <*> label "seeders"        getWord32be
                                  <*> label "peers"          getMany
            2 -> ScrapeResponse <$> label "transaction_id" getWord32be
                                <*> label "scrapes"        getMany
            3 -> ErrorResponse <$> label "transaction_id" getWord32be
                               <*> label "message"        (fmap BL.toStrict getRemainingLazyByteString)
            _ -> fail "Unknown action"

    put = undefined

instance Binary Event where
    get = do
        num <- getWord32be
        case num of
            0 -> return None
            1 -> return Completed
            2 -> return Started
            3 -> return Stopped
            _ -> fail "Unknown event"

    put None      = putWord32be 0
    put Completed = putWord32be 1
    put Started   = putWord32be 2
    put Stopped   = putWord32be 3

instance Binary Scrape where
    get = Scrape <$> get <*> get <*> get

    put = undefined

getMany :: Binary a => Get [a]
getMany = getManyWith get

getManyWith :: Get a -> Get [a]
getManyWith f = do
    empty <- isEmpty
    if empty
        then return []
        else (:) <$> f <*> getManyWith f

putMany :: Binary a => [a] -> Put
putMany = putManyWith put

putManyWith :: (a -> Put) -> [a] -> Put
putManyWith = mapM_

data TrackState = TrackState { tsSock :: !Socket
                             , tsTransactionId :: !Word32
                             , tsConnectionId :: !Word64
                             }

type TrackerMonad = S.StateT TrackState IO

evalTracker :: Socket -> TrackerMonad a -> IO a
evalTracker sock x = S.evalStateT x (TrackState sock 0 0)

nextTransaction :: TrackerMonad Word32
nextTransaction = do
    state <- S.get
    S.put $ state {tsTransactionId = tsTransactionId state + 1}
    return $ tsTransactionId state

sendRequest :: UdpTrackerRequest -> TrackerMonad ()
sendRequest request = do
    sock <- S.gets tsSock
    void $ liftIO $ send sock $ BL.toStrict $ encode request

receiveResponse :: TrackerMonad UdpTrackerResponse
receiveResponse = do
    sock <- S.gets tsSock
    -- 1220 is enough for 200 peers in AnnounceResponse
    liftIO $ decode . BL.fromStrict <$> recv sock 1220

getConnectionId :: TrackerMonad Word64
getConnectionId = S.gets tsConnectionId

runTransaction :: (Word64 -> Word32 -> UdpTrackerRequest) -> TrackerMonad UdpTrackerResponse
runTransaction packet = do
    transaction <- nextTransaction
    connectionId <- getConnectionId
    sendRequest $ packet connectionId transaction
    waitForResponse transaction

waitForResponse :: Word32 -> TrackerMonad UdpTrackerResponse
waitForResponse transaction = do
    resp <- receiveResponse
    if tpTransactionId resp == transaction
        then return resp
        else waitForResponse transaction

updater :: PeerId -> Torrent -> URI -> IO TrackerResponse
updater peerId torrent uri = do
    bracket (openSocket uri) close $ \sock ->
        evalTracker sock $ do
            sendConnectRequest
            connectionId <- getConnectionId
            liftIO $ debugM "HTorrent.Tracker.UDP" $ "Connection id: " ++ show connectionId
            AnnounceResponse{..} <- sendAnnounce peerId torrent
            return $ TrackerResponse{ rInterval = fromIntegral apInterval, rPeers = apPeers }

openSocket :: URI -> IO Socket
openSocket uri = do
    addrInfo <- getAddrInfo Nothing (uriRegName uri) (fmap show (uriPort uri) `mplus` Just "6969")
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    connect sock (addrAddress serverAddr)
    return sock

sendConnectRequest :: TrackerMonad UdpTrackerResponse
sendConnectRequest = do
    resp@ConnectResponse{ cpConnectionId = connectionId } <- runTransaction (const ConnectRequest)
    S.modify' $ \s -> s{ tsConnectionId = connectionId }
    return resp

sendAnnounce :: PeerId -> Torrent -> TrackerMonad UdpTrackerResponse
sendAnnounce (PeerId peerId) torrent =
        runTransaction $ \connectionId transactionId ->
            AnnounceRequest
                { aqConnectionId = connectionId
                , aqTransactionId = transactionId
                , aqInfoHash = infoHash
                , aqPeerId = peerId
                , aqDownloaded = 0
                , aqLeft = 0
                , aqUploaded = 0
                , aqEvent = None
                , aqIpAddress = 0
                , aqKey = 0
                , aqNumWant = 200
                , aqPort = 0
                }
    where infoHash = tInfoHash torrent
