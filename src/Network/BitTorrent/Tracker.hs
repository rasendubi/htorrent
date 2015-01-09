{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.Tracker
    ( announceURI
    ) where

import Data.ByteString.Char8 as BS
import Data.Torrent
import Network.HTTP.Types.URI

announceURI :: Torrent -> ByteString -> String
announceURI t peerId = BS.unpack $ tAnnounce t `BS.append` renderSimpleQuery True fields
    where
        fields =
            [ ("info_hash", getInfoHash t)
            , ("peer_id", peerId)
            ]
