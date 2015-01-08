module Network.BitTorrent.Tracker
    ( announceURI
    ) where

import Data.ByteString.Char8 as BS
import Data.Torrent
import Network.HTTP

announceURI :: Torrent -> String
announceURI t = (BS.unpack $ tAnnounce t) ++ '?' : urlEncodeVars fields
    where
        fields =
            [ ("info_hash", BS.unpack $ getInfoHash t)
            ]
