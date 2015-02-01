{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.Peer
( handshake
, connectToPeer
, sendHandshake
, Handshake(..)
, Message(..)
, toMessages
, socketToMessages
, sendMessage
, lengthToRequests
)
where

import Debug.Trace

import Network.BitTorrent.Tracker (Peer(..))

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import Network.Socket.ByteString.Lazy

import qualified Network.Socket.ByteString.Lazy as SL
import qualified Network.Socket.ByteString as S

import Control.Exception

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import Control.Applicative

import System.Log.Logger

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
    | Choke
    | Unchoke
    | Interested
    | NotInterested
    | Have Word32
    | Bitfield ByteString
    | Request
        { rIndex :: Word32
        , rBegin :: Word32
        , rLength :: Word32
        }
    | Piece
        { pIndex :: Word32
        , pBegin :: Word32
        , pBlock :: ByteString
        }
    deriving (Show, Read, Eq)

instance Binary Message where
    put KeepAlive = putWord32be 0
    put Choke = putWord32be 1 >> putWord8 0
    put Unchoke = putWord32be 1 >> putWord8 1
    put Interested = putWord32be 1 >> putWord8 2
    put NotInterested = putWord32be 1 >> putWord8 3
    put (Have idx) = putWord32be 5 >> putWord8 4 >> putWord32be idx
    put (Bitfield bitfield) = do
        putWord32be . fromIntegral $ 1 + BS.length bitfield
        putWord8 5
        putByteString bitfield
    put (Request index begin len) = do
        putWord32be 13
        putWord8 6
        putWord32be index
        putWord32be begin
        putWord32be len
    put (Piece index begin block) = do
        putWord32be . fromIntegral $ 9 + BS.length block
        putWord8 7
        putWord32be index
        putWord32be begin
        putByteString block

    get = do
        len <- getWord32be
        if len == 0
            then return KeepAlive
            else do
                msgType <- getWord8
                case msgType of
                    0 -> do
                        guard $ len == 1
                        return Choke
                    1 -> do
                        guard $ len == 1
                        return Unchoke
                    2 -> do
                        guard $ len == 1
                        return Interested
                    3 -> do
                        guard $ len == 1
                        return NotInterested
                    4 -> do
                        guard $ len == 5
                        Have <$> getWord32be
                    5 -> Bitfield <$> getByteString (fromIntegral len - 1)
                    6 -> do
                        guard $ len == 13
                        Request <$> getWord32be <*> getWord32be <*> getWord32be
                    7 -> Piece <$> getWord32be <*> getWord32be <*> getByteString (fromIntegral len - 9)
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
    infoM "HTorrent.Peer" $ "Connecting " ++ show sock ++ "(" ++ show (addrAddress addr) ++ ")"
    res <- try (connect sock (addrAddress addr)) :: IO (Either IOException ())
    return $ either (Left . show) (Right . const sock) res

sendHandshake :: ByteString -> ByteString -> Socket -> IO ()
sendHandshake peerId infoHash sock = S.sendAll sock $ handshake peerId infoHash

toMessages :: BL.ByteString -> [Either String Message]
toMessages str
    | BL.null str = []
    | otherwise   = msg : toMessages rest
    where
        (msg,rest) = case runGetOrFail get str of
            Left (_,_,err)  -> (Left err, BL.empty)
            Right (s,_,res) -> (Right res, s)

socketToMessages :: Socket -> IO [Either String Message]
socketToMessages = fmap toMessages . SL.getContents

sendMessage :: Message -> Socket -> IO ()
sendMessage msg sock = sendAll sock $ encode msg

lengthToRequests :: Integer -> Integer -> [Message]
lengthToRequests pieceSize size = recurse 0
    where
        recurse start
            | start >= size = []
            | otherwise     = Request index begin (fromIntegral len) : recurse (start + len)
            where
                index = fromIntegral $ start `div` pieceSize
                begin = fromIntegral $ start `mod` pieceSize
                len   = min 16384 (size - start)
