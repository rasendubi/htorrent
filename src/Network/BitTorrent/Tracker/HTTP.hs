{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.BitTorrent.Tracker.HTTP
    ( updater
    ) where

import Control.Applicative
import Control.Monad

import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get
import Data.BEncode as BE
import Data.BEncode.BDict as BE
import Network.HTTP (simpleHTTP, getRequest, rspBody)
import Network.HTTP.Types.URI

import System.Log.Logger
import System.IO.Error (tryIOError)

import Text.URI

import Data.Torrent
import Network.BitTorrent.Tracker.Types

newtype HttpResponse = HttpResponse { trackerResponse :: TrackerResponse }

instance BEncode HttpResponse where
    fromBEncode d@(BDict dict) = HttpResponse <$> case BE.lookup "failure reason" dict of
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
                peer = runGet get x
                x = BL.fromStrict $ BS.take 6 s
                xs = BS.drop 6 s
bencodeToPeerList _ = fail "Can't parse peer list"


announceParams :: Torrent -> PeerId -> BS.ByteString
announceParams t peerId = renderSimpleQuery True
    [ ("info_hash", getInfoHash t)
    , ("peer_id", fromPeerId peerId)
    ]

decodeResponse :: BS.ByteString -> BE.Result TrackerResponse
decodeResponse = fmap trackerResponse . BE.decode

updater :: PeerId -> Torrent -> URI -> IO TrackerResponse
updater clientId torrent announce = do
    let uri = show announce ++ BS.unpack (announceParams torrent clientId)
    infoM "HTorrent.Tracker.HTTP" $ "announcing to " ++ show uri
    resp <- tryIOError (simpleHTTP $ getRequest uri)
    case resp of
        Left err -> return . Failure . BS.pack $ show err
        Right res -> return $ either (Failure . BS.pack) id $
            case res of
                Left err -> Left $ show err
                Right rsp -> decodeResponse . BS.pack $ rspBody rsp
