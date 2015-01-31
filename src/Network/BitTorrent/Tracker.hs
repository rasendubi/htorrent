module Network.BitTorrent.Tracker
    ( module Network.BitTorrent.Tracker.Types
    , pollTracker
    ) where

import qualified Data.ByteString as BS

import Data.Torrent
import Network.BitTorrent.Tracker.Types
import qualified Network.BitTorrent.Tracker.HTTP as HTTP
import qualified Network.BitTorrent.Tracker.UDP as UDP

pollTracker :: BS.ByteString -> Torrent -> BS.ByteString -> IO TrackerState
pollTracker = HTTP.pollTracker
