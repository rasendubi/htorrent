{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.Peer
( handshake
, connectToPeer
, sendHandshake
)
where

import Network.BitTorrent.Tracker (Peer(..))

import Data.ByteString(ByteString, append)
import qualified Data.ByteString.Char8 as BS
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)

import Control.Exception

handshake :: ByteString -> ByteString -> ByteString
handshake peerId infoHash =
    "\19BitTorrent protocol\0\0\0\0\0\0\0\0" `append` infoHash `append` peerId

connectToPeer :: Peer -> IO (Either String Socket)
connectToPeer peer = do
    addrInfo <- getAddrInfo Nothing (Just . BS.unpack $ pIp peer) (Just . show $ pPort peer)
    let addr = head addrInfo
    sock <- socket (addrFamily addr) Stream defaultProtocol
    putStrLn $ "Connecting " ++ show sock ++ "(" ++ (show $ addrAddress addr) ++ ")"
    res <- try (connect sock (addrAddress addr)) :: IO (Either IOException ())
    return $ either (Left . show) (Right . const sock) res

sendHandshake :: ByteString -> ByteString -> Socket -> IO Int
sendHandshake peerId infoHash sock = send sock $ handshake peerId infoHash

