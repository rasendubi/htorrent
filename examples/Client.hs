{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Data.ByteString.Char8 as BS

import Control.Concurrent
import Control.Concurrent.STM
import Data.Torrent as T
import Network.BitTorrent.Client
import Control.Monad
import System.Exit
import System.Environment

import Control.Monad.Trans.Either
import Control.Monad.Trans

import System.Log.Logger

import System.Console.ANSI

import Data.Function (on)
import qualified Data.Array.BitArray as A
import Data.Ix

main :: IO ()
main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    hideCursor
    clearScreen
    args <- getArgs
    client <- createClient "HASKELL7TORRENT5YEAH"
    case args of
        [] -> showUsage >> exitFailure
        _ -> return ()
    forM_ args $ \file -> runEitherT $ do
        torrent <- hoistEither =<< lift (T.fromFile file)
        download <- lift $ startDownload client torrent (BS.unpack . idName . tInfoDict $ torrent)
        uiLock <- liftIO $ newMVar ()
        liftIO $ forkIO $ trackTVarChanges (activePeers client) $ onActivePeersChanged uiLock
        liftIO $ forkIO $ trackChanges (getPresentArray download) $ updatePresentArray uiLock
    forever yield

instance (Ix i) => Eq (A.BitArray i) where
    (==) = (==) `on` A.assocs

showUsage :: IO ()
showUsage = getProgName >>= \name -> putStrLn $ "Usage: " ++ name ++ " <torrent>"

updatePresentArray :: MVar () -> PiecesPresentArray -> IO ()
updatePresentArray lock a = do
    withMVar lock $ \_ -> do
        setCursorPosition 1 0
        putStrLn $ map (\x -> if x then '1' else '_') $ A.elems a
        clearFromCursorToScreenEnd

onActivePeersChanged :: MVar () -> Int -> IO ()
onActivePeersChanged lock n = do
    withMVar lock $ \_ -> do
        setCursorPosition 0 0
        clearLine
        putStr "Active peers: " >> print n
        setCursorPosition 100 0

trackTVarChanges :: (Eq a) => TVar a -> (a -> IO b) -> IO ()
trackTVarChanges var = trackChanges $ readTVar var

trackChanges :: (Eq a) => STM a -> (a -> IO b) -> IO ()
trackChanges getValue action = do
    initial <- atomically $ getValue
    action initial
    trackFurther initial getValue action

trackFurther :: (Eq a) => a -> STM a -> (a -> IO b) -> IO ()
trackFurther prev getValue action = do
    value <- atomically $ do
        val <- getValue
        if val == prev
            then retry
            else return val
    action value
    trackFurther prev getValue action
