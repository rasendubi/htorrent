{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker.UDP
    ( updater
    ) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent.STM

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Text.URI

import Data.Torrent
import Network.BitTorrent.Tracker.Types

import System.Log.Logger

data UdpTrackerRequest
    = ConnectRequest
        { cqConnectionId :: !Word64
        , cqTransactionId :: !Word32
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

data UdpTrackerResponse
    = ConnectResponse
        { cpTransactionId :: !Word32
        , cpConnectionId :: !Word64
        }
    | AnnounceResponse
        { apTransactionId :: !Word32
        , apInterval :: !Word32
        , apLeechers :: !Word32
        , apSeeders :: !Word32
        , apPeers :: [Peer]
        }
    | ScrapeResponse
        { spTransactionId :: !Word32
        , spScrapes :: [Scrape]
        }
    | ErrorResponse
        { epTransactionId :: !Word32
        , epMessage :: BS.ByteString
        }

data Scrape = Scrape { sSeeders :: !Word32
                     , sCompleted :: !Word32
                     , sLeechers :: !Word32
                     }

data Event = None | Completed | Started | Stopped

instance Binary UdpTrackerRequest where
    put ConnectRequest{..} = do
        putWord64be cqConnectionId
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

updater :: PeerId -> Torrent -> URI -> IO TrackerResponse
updater peerId torrent uri = return $ Failure "UDP not implemented"
