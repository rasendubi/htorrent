{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.Tracker
    ( module Network.BitTorrent.Tracker.Types
    , TrackerState(..)
    , pollTracker
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay

import qualified Data.ByteString.Char8 as BS

import Data.Torrent
import Network.BitTorrent.Tracker.Types
import qualified Network.BitTorrent.Tracker.HTTP as HTTP
import qualified Network.BitTorrent.Tracker.UDP as UDP

import Text.URI (URI(..), parseURI)

import System.Log.Logger

data TrackerState = TrackerState
    { tsTracker :: BS.ByteString
    , tsTrackerResponse :: TVar TrackerResponse
    , tsUpdateDelay :: TVar (Maybe Delay)
    }

pollTracker :: PeerId -> Torrent -> BS.ByteString -> IO TrackerState
pollTracker peerId torrent url = tracker url peerId torrent

tracker :: BS.ByteString -> PeerId -> Torrent -> IO TrackerState
tracker urlString peerId torrent =
    case parseURI (BS.unpack urlString) of
        Nothing -> updaterError urlString "Invalid URL"
        Just url ->
            case uriScheme url of
                Just "http" -> pollTracker' url $ HTTP.updater peerId torrent url
                Just "udp"  -> pollTracker' url $ UDP.updater  peerId torrent url
                _           -> updaterError urlString "Unknown Protocol"

pollTracker' :: URI -> IO TrackerResponse -> IO TrackerState
pollTracker' announce updater = do
    state <- TrackerState (BS.pack $ show announce)
        <$> newTVarIO (TrackerFailure $ "Not Requested")
        <*> (newTVarIO . Just =<< newDelay 0)
    forkIO $ updateTracker state updater
    return state

updateTracker :: TrackerState -> IO TrackerResponse -> IO ()
updateTracker (TrackerState _ responseVar delayVar) updater = forever $ do
        atomically waitUpdate
        response <- updater
        d <- newDelay $ 1000000 * case response of
                TrackerFailure _ -> 20
                TrackerResponse interval _ -> fromIntegral interval
        atomically $ do
            writeTVar delayVar (Just d)
            writeTVar responseVar response
        infoM "HTorrent.Tracker" $ show response
    where
        waitUpdate = do
            mDelay <- readTVar delayVar
            case mDelay of
                Nothing -> retry
                Just delay -> waitDelay delay
            writeTVar delayVar Nothing

updaterError :: BS.ByteString -> BS.ByteString -> IO TrackerState
updaterError uri reason =
    TrackerState uri
        <$> newTVarIO (TrackerFailure reason)
        <*> newTVarIO Nothing
