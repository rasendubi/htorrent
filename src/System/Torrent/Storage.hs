{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module System.Torrent.Storage
    ( Storage
    , PiecesPresentArray
    , fromTorrent
    , createStorage
    , writePiece
    , getUnfinishedPieces
    , containsNeededPieces
    ) where

import Prelude hiding (zipWith, or)

import Control.Applicative ((<$>),(<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import qualified Crypto.Hash.SHA1 as SHA1

import Data.Array.BitArray
import qualified Data.ByteString.Char8 as BS
import Data.Word

import System.Directory (createDirectoryIfMissing)
import System.IO (withBinaryFile, hSeek, IOMode(..), SeekMode(..))

import Data.Torrent

data Storage = Storage
    { sTorrent :: Torrent
    , sPath :: FilePath
    , sPiecesPresent :: TVar PiecesPresentArray
    , sWriterChan :: TChan (Word32, BS.ByteString)
    }

type PiecesPresentArray = BitArray Word32

createStorage :: Torrent -> FilePath -> IO Storage
createStorage torrent path = do
    storage <- Storage torrent path <$> newTVarIO (bitArrayForTorrent torrent) <*> newTChanIO
    forkIO $ writer storage
    return storage

fromTorrent :: Torrent -> IO Storage
fromTorrent torrent = createStorage torrent (BS.unpack . idName . tInfoDict $ torrent)

writeBlock :: Storage -> Word32 -> BS.ByteString -> IO ()
writeBlock = undefined

bitArrayForTorrent :: Torrent -> BitArray Word32
bitArrayForTorrent t = array (0, numPieces t - 1) []

writePiece :: Storage -> Word32 -> BS.ByteString -> STM ()
writePiece Storage{..} idx piece = do
    writeTChan sWriterChan (idx, piece)
    modifyTVar' sPiecesPresent (// [(idx, True)])

writer :: Storage -> IO ()
writer storage@Storage{ sWriterChan = channel } = do
    (idx, block) <- atomically $ readTChan channel
    forkIO $ writePiece' storage idx block
    writer storage

writePiece' :: Storage -> Word32 -> BS.ByteString -> IO ()
writePiece' s@Storage{..} idx piece = do
    putStrLn $ "Writing to index: " ++ show idx
    let pieceSize = idPieceLength $ tInfoDict sTorrent
    let realPieceHash = SHA1.hash piece
    let expectedPieceHash = pieceHash sTorrent (fromIntegral idx)
    if realPieceHash == expectedPieceHash
        then writeBlockToStorage s piece (fromIntegral idx * pieceSize)
        else atomically $ modifyTVar' sPiecesPresent (// [(idx, False)])

writeBlockToStorage :: Storage -> BS.ByteString -> Integer -> IO ()
writeBlockToStorage s@Storage{..} piece pos
    | BS.null piece = return ()
    | otherwise     = do
        let (fileChain, offset, maxLen) = offsetToFile sTorrent pos
        let pathChain = BS.pack sPath : fileChain
        let filePath = BS.unpack . BS.intercalate "/" $ pathChain
        let dirPath = BS.unpack . BS.intercalate "/" $ init pathChain
        createDirectoryIfMissing True dirPath
        let (this,next) = BS.splitAt (fromIntegral maxLen) piece
        withBinaryFile filePath ReadWriteMode $ \h -> do
            hSeek h AbsoluteSeek offset
            BS.hPut h this
        writeBlockToStorage s next (pos + maxLen)

offsetToFile :: Torrent -> Integer -> ([BS.ByteString], Integer, Integer)
offsetToFile torrent offset =
        case rest of
            []          -> ([], offset, totalLength $ tInfoDict torrent)
            ((fo,fi):_) -> (fiPath fi, offset - fo, fiLength fi - (offset - fo))
    where
        rest = dropWhile ((> offset) . fst) $ reverse $ filesWithOffsets torrent

filesWithOffsets :: Torrent -> [(Integer, FileInfo)]
filesWithOffsets Torrent{ tInfoDict = InfoDict{ idFiles = Nothing } } = []
filesWithOffsets Torrent{ tInfoDict = InfoDict{ idFiles = Just files } } =
    enumerateFiles 0 files

enumerateFiles :: Integer -> [FileInfo] -> [(Integer, FileInfo)]
enumerateFiles _ [] = []
enumerateFiles offset (x:xs) = (offset, x) : enumerateFiles (offset + fiLength x) xs

getUnfinishedPieces :: Storage -> STM [Word32]
getUnfinishedPieces Storage{ sPiecesPresent = piecesPresentVar } = do
    piecesPresent <- readTVar piecesPresentVar
    return . fmap fst . filter (not.snd) $ assocs piecesPresent

containsNeededPieces :: Storage -> PiecesPresentArray -> STM Bool
containsNeededPieces storage peerPieces = do
    ourPieces <- readTVar $ sPiecesPresent storage
    return $ or $ zipWith (\x y -> y && not x) ourPieces peerPieces
