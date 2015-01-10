{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.Peer
( handshake
, connectToPeer
, sendHandshake
, Handshake(..)
, Message(..)
, toMessages
, socketToMessages
)
where

import Debug.Trace

import Network.BitTorrent.Tracker (Peer(..))

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import Network.Socket.ByteString (send)

import qualified Network.Socket.ByteString.Lazy as SL

import Control.Exception

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import Control.Applicative

data Handshake = Handshake
                { hExtensionBits :: Word64
                , hInfoHash :: ByteString
                , hPeerId :: ByteString
                } deriving (Show, Read, Eq)

instance Binary Handshake where
    put (Handshake extension infoHash peerId) = do
        putWord8 19
        putByteString "BitTorrent protocol"
        putWord64be extension
        putByteString infoHash
        putByteString peerId
    get = do
        guard . (=="\19BitTorrent protocol") =<< getByteString 20
        Handshake <$> getWord64be <*> getByteString 20 <*> getByteString 20

data Message
    = KeepAlive
    | Have Word32
    | Bitfield ByteString
    deriving (Show, Read, Eq)

instance Binary Message where
    put KeepAlive = putWord32be 0
    put (Have idx) = putWord32be 5 >> putWord8 4 >> putWord32be idx
    put (Bitfield bitfield) = do
        putWord32be . fromIntegral $ 1 + BS.length bitfield
        putWord8 5
        putByteString bitfield
    get = do
        len <- getWord32be
        if len == 0
            then return KeepAlive
            else do
                msgType <- getWord8
                case msgType of
                    4 -> do
                        guard $ len == 5
                        Have <$> getWord32be
                    5 -> Bitfield <$> getByteString (fromIntegral len - 1)
                    _ -> do
                        rest <- getByteString $ fromIntegral len - 1
                        traceShow rest $ fail "Can't parse message"

handshake :: ByteString -> ByteString -> ByteString
handshake peerId infoHash =
    BL.toStrict . encode $ Handshake 0 infoHash peerId

connectToPeer :: Peer -> IO (Either String Socket)
connectToPeer peer = do
    addrInfo <- getAddrInfo Nothing (Just . BS.unpack $ pIp peer) (Just . show $ pPort peer)
    let addr = head addrInfo
    sock <- socket (addrFamily addr) Stream defaultProtocol
    putStrLn $ "Connecting " ++ show sock ++ "(" ++ show (addrAddress addr) ++ ")"
    res <- try (connect sock (addrAddress addr)) :: IO (Either IOException ())
    return $ either (Left . show) (Right . const sock) res

sendHandshake :: ByteString -> ByteString -> Socket -> IO Int
sendHandshake peerId infoHash sock = send sock $ handshake peerId infoHash

toMessages :: BL.ByteString -> [Either String Message]
toMessages str
    | BL.null str = []
    | otherwise   = traceShow msg msg : toMessages rest
    where
        (msg,rest) = case runGetOrFail get str of
            Left (s,_,err)  -> (Left err, s)
            Right (s,_,res) -> (Right res, s)

socketToMessages :: Socket -> IO [Either String Message]
socketToMessages = fmap toMessages . SL.getContents
