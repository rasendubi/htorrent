{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Torrent as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Network.BitTorrent.Client
import Network.BitTorrent.Tracker
import Network.BitTorrent.Peer
import Network.HTTP hiding (Response,Request)
import Network.Socket.ByteString (recv)
import Network.Socket hiding (recv)
import Control.Monad
import System.Exit
import System.Environment
import Data.Binary
import Control.Applicative
import System.IO
import Data.Function

import Control.Monad.Trans.Either
import Control.Monad.Trans

main :: IO ()
main = do
    args <- getArgs
    client <- Client "HASKELL7TORRENT5YEAH" <$> newTVarIO 0
    case args of
        [] -> showUsage >> exitFailure
        _ -> return ()
    forM_ args $ \file -> runEitherT $ do
        torrent <- hoistEither =<< lift (T.fromFile file)
        lputStrLn $ file ++ ": " ++ show torrent
        lift $ startDownload client torrent
    forever yield

onTrackerResponseChanged resp = do
    putStr "Tracker changed: "
    print resp
    
trackChanges :: (Eq a) => (a -> IO b) -> TVar a -> IO ()
trackChanges action var = do
    initial <- readTVarIO var
    action initial
    trackFurther action var initial

trackFurther :: (Eq a) => (a -> IO b) -> TVar a -> a -> IO ()
trackFurther action var prev = do
    value <- atomically $ do
        val <- readTVar var
        if val == prev
            then retry
            else return val
    action value
    trackFurther action var value

handleMessage :: Socket -> Torrent -> Either String Message -> IO ()
handleMessage _ _ (Left str) = putStrLn $ "Error: " ++ str
handleMessage conn torrent (Right msg) = do
    print msg
    let requests = lengthToRequests (idPieceLength . tInfoDict $ torrent) (totalLength . tInfoDict $ torrent)
    let pieceSize = idPieceLength . tInfoDict $ torrent
    case msg of
        Unchoke -> forM_ requests $ \m -> sendMessage m conn
        Piece index begin piece -> withBinaryFile (BS.unpack . idName . tInfoDict $ torrent) ReadWriteMode $ \h -> do
            hSeek h AbsoluteSeek (fromIntegral index * pieceSize + fromIntegral begin)
            BS.hPut h piece
        _ -> return ()

lprint :: (Show a) => a -> EitherT String IO ()
lprint = lift . print

lputStr :: String -> EitherT String IO ()
lputStr = lift . putStr

lputStrLn :: String -> EitherT String IO ()
lputStrLn = lift . putStrLn

showUsage :: IO ()
showUsage = getProgName >>= \name -> putStrLn $ "Usage: " ++ name ++ " <torrent>"

peerId :: BS.ByteString
peerId = undefined
