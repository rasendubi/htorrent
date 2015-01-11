{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.BitTorrent.Tracker
    ( announceURI
    , announceParams
    , TrackerResponse(..)
    , Peer(..)
    , decodeResponse
    ) where

import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy as BL
import Data.Torrent
import Network.HTTP.Types.URI
import Data.Binary.Get
import Data.BEncode as BE
import Data.BEncode.BDict as BE
import Data.Word
import Data.Typeable

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
            interval <- field (req "interval")
            peers <- bencodeToPeerList =<< req "peers"
            return $ Response interval peers
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
wordToStr = BS.pack . show . (fromIntegral :: Word8 -> Int)

announceURI :: Torrent -> BS.ByteString -> String
announceURI t peerId = BS.unpack $ tAnnounce t `BS.append` announceParams t peerId

announceParams :: Torrent -> BS.ByteString -> BS.ByteString
announceParams t peerId = renderSimpleQuery True
    [ ("info_hash", getInfoHash t)
    , ("peer_id", peerId)
    ]

decodeResponse :: BS.ByteString -> BE.Result TrackerResponse
decodeResponse = BE.decode
