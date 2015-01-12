{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Torrent
    ( Torrent(..)
    , InfoDict(..)
    , FileInfo(..)
    , fromFile
    , getInfoHash
    , toHex
    , totalLength
    , numPieces
    ) where

import qualified Crypto.Hash.SHA1 as SHA1
import Data.BEncode as BE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Typeable
import Text.Printf
import Data.List (foldl')

data Torrent = Torrent
    { tAnnounce :: BS.ByteString
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
    fromBEncode = fromDict $
        Torrent <$>! "announce"
                <*>! "info"
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

getInfoHash :: Torrent -> BS.ByteString
getInfoHash = SHA1.hashlazy . BE.encode . tInfoDict

toHex :: BS.ByteString -> String
toHex bytes = printf "%02x" =<< C.unpack bytes

totalLength :: InfoDict -> Integer
totalLength InfoDict { idLength = Just len } = len
totalLength InfoDict { idFiles = Just files } = sum' $ map fiLength files
    where sum' = foldl' (+) 0
totalLength _ = undefined

numPieces :: Num a => Torrent -> a
numPieces t = fromIntegral $ (totalLen + pieceLen - 1) `div` pieceLen
    where
        totalLen = totalLength $ tInfoDict t
        pieceLen = idPieceLength $ tInfoDict t
