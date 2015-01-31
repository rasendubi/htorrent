{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.Tracker.Types where

import Control.Applicative ((<$>), (<*>))

import Data.Binary
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.Typeable

data TrackerResponse
    = TrackerFailure { fReason :: BS.ByteString }
    | TrackerResponse
        { rInterval :: Integer
        , rPeers :: [Peer]
        }
    deriving (Read, Show, Eq, Typeable)

data Peer = Peer
    { pPeerId :: Maybe PeerId
    , pIp :: BS.ByteString
    , pPort :: Word16
    }
    deriving (Read, Show, Eq, Typeable)

newtype PeerId = PeerId { fromPeerId :: BS.ByteString }
    deriving (Read, Show, Eq, Typeable)

instance Binary Peer where
    get = Peer Nothing <$> fmap showIp getWord32be <*> getWord16be

    put = undefined

showIp :: Word32 -> BS.ByteString
showIp x = BS.intercalate "." [ wordToStr $ fromIntegral (x `shiftR` n) | n <- [24, 16, 8, 0]]
    where
        wordToStr :: Word8 -> BS.ByteString
        wordToStr = BS.pack . show
