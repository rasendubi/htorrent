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
    , offsetToFile
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

filesWithOffsets :: Torrent -> [(Integer, FileInfo)]
filesWithOffsets Torrent{ tInfoDict = InfoDict{ idFiles = Nothing } } = []
filesWithOffsets Torrent{ tInfoDict = InfoDict{ idFiles = Just files } } =
    enumerateFiles 0 files

enumerateFiles :: Integer -> [FileInfo] -> [(Integer, FileInfo)]
enumerateFiles _ [] = []
enumerateFiles offset (x:xs) = (offset, x) : enumerateFiles (offset + fiLength x) xs

offsetToFile :: Torrent -> Integer -> ([BS.ByteString], Integer, Integer)
offsetToFile torrent offset =
        case rest of
            []          -> ([], offset, totalLength $ tInfoDict torrent)
            ((fo,fi):_) -> (fiPath fi, offset - fo, fiLength fi - (offset - fo))
    where
        rest = dropWhile ((> offset) . fst) $ reverse $ filesWithOffsets torrent
