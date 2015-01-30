{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.BitTorrent.Tracker.HTTP
    ( TrackerState(..)
    , TrackerResponse(..)
    , Peer(..)
    , pollTracker
    ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay

import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.BEncode as BE
import Data.BEncode.BDict as BE
import Data.Word
import Data.Typeable
import Network.HTTP (simpleHTTP, getRequest, rspBody)
import Network.HTTP.Types.URI

import System.Log.Logger
import System.IO.Error (tryIOError)

import Data.Torrent

data TrackerState = TrackerState
    { tsTracker :: BS.ByteString
    , tsTrackerResponse :: TVar TrackerResponse
    , tsUpdateDelay :: TVar (Maybe Delay)
    }

data TrackerResponse
    = Failure { fReason :: BS.ByteString }
    | Response
        { rInterval :: Integer
        , rPeers :: [Peer]
        }
    deriving (Read, Show, Eq, Typeable)

data Peer = Peer
    { pPeerId :: Maybe BS.ByteString
    , pIp :: BS.ByteString
    , pPort :: Integer
    }
    deriving (Read, Show, Eq, Typeable)

instance BEncode TrackerResponse where
    fromBEncode d@(BDict dict) = case BE.lookup "failure reason" dict of
        Just (BString str) -> return $ Failure str
        Nothing -> flip fromDict d $ do
            Response <$> field (req "interval") <*> (bencodeToPeerList =<< req "peers")
        _ -> fail "Failure reason should be string"
    fromBEncode _ = fail "Tracker response should be dictionary"

    toBEncode = undefined

bencodeToPeerList :: BValue -> BE.Get [Peer]
bencodeToPeerList (BString str) = return $ stringToPeers str
    where
        stringToPeers s
            | BS.null s = []
            | otherwise = peer : stringToPeers xs
            where
                peer = runGet parsePeer x
                x = BL.fromStrict $ BS.take 6 s
                xs = BS.drop 6 s
bencodeToPeerList _ = fail "Can't parse peer list"

parsePeer :: Data.Binary.Get.Get Peer
parsePeer = do
    ip1 <- getWord8
    ip2 <- getWord8
    ip3 <- getWord8
    ip4 <- getWord8
    let ip = BS.intercalate "." $ fmap wordToStr [ip1, ip2, ip3, ip4]
    port <- getWord16be
    return $ Peer Nothing ip (fromIntegral port)

wordToStr :: Word8 -> BS.ByteString
wordToStr = BS.pack . show

announceURI :: Torrent -> BS.ByteString -> String
announceURI t peerId = BS.unpack $ tAnnounce t `BS.append` announceParams t peerId

announceParams :: Torrent -> BS.ByteString -> BS.ByteString
announceParams t peerId = renderSimpleQuery True
    [ ("info_hash", getInfoHash t)
    , ("peer_id", peerId)
    ]

decodeResponse :: BS.ByteString -> BE.Result TrackerResponse
decodeResponse = BE.decode

pollTracker :: BS.ByteString -> Torrent -> BS.ByteString -> IO TrackerState
pollTracker clientId torrent announce = do
    state <- TrackerState announce
        <$> newTVarIO (Failure $ BS.pack "Not requested")
        <*> (newTVarIO . Just =<< newDelay 0)
    forkIO $ updateTracker clientId torrent state
    return state

updateTracker :: BS.ByteString -> Torrent -> TrackerState -> IO ()
updateTracker clientId torrent (TrackerState tracker responseVar delayVar) = forever $ do
        atomically waitUpdate
        response <- announceToURI clientId torrent tracker
        d <- newDelay $ 1000000 * case response of
                Failure _ -> 20
                Response interval _ -> fromIntegral interval
        atomically $ do
            writeTVar delayVar (Just d)
            writeTVar responseVar response
        infoM "HTorrent.Client" $ show response
    where
        waitUpdate = do
            mDelay <- readTVar delayVar
            case mDelay of
                Nothing -> retry
                Just delay -> waitDelay delay
            writeTVar delayVar Nothing

announceToURI :: BS.ByteString -> Torrent -> BS.ByteString -> IO TrackerResponse
announceToURI clientId torrent announce = do
    let uri = BS.unpack $ announce `BS.append` announceParams torrent clientId
    infoM "HTorrent.Client" $ "announcing to " ++ show uri
    resp <- tryIOError (simpleHTTP $ getRequest uri)
    case resp of
        Left err -> return . Failure . BS.pack $ show err
        Right res -> return $ either (Failure . BS.pack) id $
            case res of
                Left err -> Left $ show err
                Right rsp -> decodeResponse . BS.pack $ rspBody rsp
