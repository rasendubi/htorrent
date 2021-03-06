{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Torrent
    ( Torrent(..)
    , InfoDict(..)
    , FileInfo(..)
    , fromFile
    , toFile
    , getInfoHash
    , toHex
    , totalLength
    , numPieces
    , pieceHash
    , pieceSize
    , pieceOffset
    ) where

import qualified Crypto.Hash.SHA1 as SHA1
import Data.BEncode as BE
import Data.BEncode.BDict as BE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import Data.Typeable
import Text.Printf
import Data.List (foldl')
import Data.Word

data Torrent = Torrent
    { tInfoHash :: BS.ByteString
    , tAnnounce :: BS.ByteString
    , tInfoDict :: InfoDict
    } deriving (Show, Read, Eq, Typeable)

data InfoDict = InfoDict
    { idFiles :: Maybe [FileInfo]
    , idLength :: Maybe Integer
    , idName :: BS.ByteString
    , idPieceLength :: Integer
    , idPieces :: BS.ByteString
    } deriving (Show, Read, Eq, Typeable)

data FileInfo = FileInfo
    { fiLength :: Integer
    , fiPath :: [BS.ByteString]
    } deriving (Show, Read, Eq, Typeable)

instance BEncode Torrent where
    fromBEncode d@(BDict dict)
        | Just info <- BE.lookup "info" dict = flip fromDict d $
            Torrent (SHA1.hashlazy $ BE.encode info)
                    <$>! "announce"
                    <*>! "info"
    fromBEncode _ = fail "info should be dict"
    toBEncode t = toDict $
           "announce" .=! tAnnounce t
        .: "info" .=! tInfoDict t
        .: endDict

instance BEncode InfoDict where
    fromBEncode = fromDict $
        InfoDict <$>? "files"
                 <*>? "length"
                 <*>! "name"
                 <*>! "piece length"
                 <*>! "pieces"
    toBEncode i = toDict $
           "files" .=? idFiles i
        .: "length" .=? idLength i
        .: "name" .=! idName i
        .: "piece length" .=! idPieceLength i
        .: "pieces" .=! idPieces i
        .: endDict

instance BEncode FileInfo where
    fromBEncode = fromDict $
        FileInfo <$>! "length"
                 <*>! "path"
    toBEncode fi = toDict $
           "length" .=! fiLength fi
        .: "path" .=! fiPath fi
        .: endDict

fromFile :: String -> IO (Either String Torrent)
fromFile = fmap BE.decode . BS.readFile

toFile :: Torrent -> String -> IO ()
toFile torrent file = BS.writeFile file . BL.toStrict $ BE.encode torrent

getInfoHash :: Torrent -> BS.ByteString
getInfoHash = tInfoHash

toHex :: BS.ByteString -> String
toHex bytes = printf "%02x" =<< C.unpack bytes

totalLength :: InfoDict -> Integer
totalLength InfoDict { idLength = Just len } = len
totalLength InfoDict { idFiles = Just files } = sum' $ fmap fiLength files
    where sum' = foldl' (+) 0
totalLength _ = undefined

numPieces :: Num a => Torrent -> a
numPieces t = fromIntegral $ (totalLen + pieceLen - 1) `div` pieceLen
    where
        totalLen = totalLength $ tInfoDict t
        pieceLen = idPieceLength $ tInfoDict t

pieceHash :: Torrent -> Word32 -> BS.ByteString
pieceHash torrent idx = BS.take 20 . BS.drop (20 * fromIntegral idx) . idPieces $ tInfoDict torrent

pieceSize :: Torrent -> Word32 -> Integer
pieceSize torrent idx = min regularSize (totalLen - regularSize * fromIntegral idx)
    where
        regularSize = idPieceLength $ tInfoDict torrent
        totalLen = totalLength $ tInfoDict torrent

pieceOffset :: Torrent -> Word32 -> Integer
pieceOffset torrent idx = fromIntegral idx * idPieceLength (tInfoDict torrent)
