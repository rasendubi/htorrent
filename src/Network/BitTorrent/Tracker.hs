module Network.BitTorrent.Tracker
    ( HTTP.TrackerState(..)
    , HTTP.TrackerResponse(..)
    , HTTP.Peer(..)
    , pollTracker
    ) where

import qualified Data.ByteString as BS

import Data.Torrent
import qualified Network.BitTorrent.Tracker.HTTP as HTTP

pollTracker :: BS.ByteString -> Torrent -> BS.ByteString -> IO HTTP.TrackerState
pollTracker = HTTP.pollTracker
