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
    , getPiecesPresent
    ) where

import Prelude hiding (zipWith, or)

import Control.Applicative ((<$>),(<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import qualified Crypto.Hash.SHA1 as SHA1

import Data.Maybe
import Data.Array.BitArray
import qualified Data.ByteString.Char8 as BS
import Data.Word

import System.Directory (createDirectoryIfMissing)
import System.IO (withBinaryFile, hSeek, IOMode(..), SeekMode(..))

import Data.Torrent

import System.Log.Logger

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
    checkStorage storage
    forkIO $ do
        writer storage
    return storage

fromTorrent :: Torrent -> IO Storage
fromTorrent torrent = createStorage torrent (BS.unpack . idName . tInfoDict $ torrent)

bitArrayForTorrent :: Torrent -> BitArray Word32
bitArrayForTorrent t = array (0, numPieces t - 1) []

writePiece :: Storage -> Word32 -> BS.ByteString -> STM ()
writePiece Storage{..} idx piece = do
    writeTChan sWriterChan (idx, piece)
    modifyTVar' sPiecesPresent (// [(idx, True)])

checkStorage :: Storage -> IO ()
checkStorage s@Storage{..} = do
    lst <- forM [0..numPieces sTorrent - 1] $ \idx -> do
        state <- checkPiece s idx
        infoM "HTorrent.Storage" $ "Checked piece " ++ show idx ++ ": " ++ show state
        return (idx, state)
    atomically $ writeTVar sPiecesPresent $ array (0, numPieces sTorrent - 1) lst

checkPiece :: Storage -> Word32 -> IO Bool
checkPiece = (fmap isJust .) . readPiece

readPiece :: Storage -> Word32 -> IO (Maybe BS.ByteString)
readPiece s@Storage{..} idx = do
    let size = pieceSize sTorrent idx
    let offset = pieceOffset sTorrent idx
    let expectedPieceHash = pieceHash sTorrent idx
    fmap BS.concat . mfilter (checkHash expectedPieceHash) . sequence <$> readBlock s offset size

checkHash :: BS.ByteString -> [BS.ByteString] -> Bool
checkHash expected lst = expected == actual
    where actual = SHA1.finalize $ SHA1.updates SHA1.init lst

readBlock :: Storage -> Integer -> Integer -> IO [Maybe BS.ByteString]
readBlock s@Storage{..} pos len
    | len <= 0  = return $ []
    | otherwise = do
        let (fileChain, offset, maxLen) = offsetToFile sTorrent pos
        let filePath = toFilePath sPath fileChain
        let realLen = min len maxLen
        mBlock <- fmap toMaybe $ try $ withBinaryFile filePath ReadMode $ \h -> do
            hSeek h AbsoluteSeek offset
            BS.hGet h (fromIntegral realLen)
        (mBlock :) <$> readBlock s (pos + realLen) (len - realLen)

toMaybe :: Either IOException b -> Maybe b
toMaybe = either (const Nothing) Just

writer :: Storage -> IO ()
writer storage@Storage{ sWriterChan = channel } = do
    (idx, block) <- atomically $ readTChan channel
    forkIO $ writePiece' storage idx block
    writer storage

writePiece' :: Storage -> Word32 -> BS.ByteString -> IO ()
writePiece' s@Storage{..} idx piece = do
    infoM "HTorrent.Storage" $ "Writing to index: " ++ show idx
    let offset = pieceOffset sTorrent idx
    let realPieceHash = SHA1.hash piece
    let expectedPieceHash = pieceHash sTorrent (fromIntegral idx)
    if realPieceHash == expectedPieceHash
        then writeBlockToStorage s piece offset
        else atomically $ modifyTVar' sPiecesPresent (// [(idx, False)])

writeBlockToStorage :: Storage -> BS.ByteString -> Integer -> IO ()
writeBlockToStorage s@Storage{..} piece pos
    | BS.null piece = return ()
    | otherwise     = do
        let (fileChain, offset, maxLen) = offsetToFile sTorrent pos
        let filePath = toFilePath sPath fileChain
        let dirPath = toDirPath sPath fileChain
        createDirectoryIfMissing True dirPath
        let (this,next) = BS.splitAt (fromIntegral maxLen) piece
        withBinaryFile filePath ReadWriteMode $ \h -> do
            hSeek h AbsoluteSeek offset
            BS.hPut h this
        writeBlockToStorage s next (pos + maxLen)

toFilePath :: FilePath -> [BS.ByteString] -> FilePath
toFilePath prefix pathChain = BS.unpack . BS.intercalate "/" $ BS.pack prefix : pathChain

toDirPath :: FilePath -> [BS.ByteString] -> FilePath
toDirPath prefix pathChain = BS.unpack . BS.intercalate "/" . init $ BS.pack prefix : pathChain

offsetToFile :: Torrent -> Integer -> ([BS.ByteString], Integer, Integer)
offsetToFile torrent offset =
        case rest of
            []          -> ([], offset, (totalLength $ tInfoDict torrent) - offset)
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

getPiecesPresent :: Storage -> STM PiecesPresentArray
getPiecesPresent = readTVar . sPiecesPresent
