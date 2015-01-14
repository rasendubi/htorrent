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

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    args <- getArgs
    client <- createClient "HASKELL7TORRENT5YEAH"
    case args of
        [] -> showUsage >> exitFailure
        _ -> return ()
    forM_ args $ \file -> runEitherT $ do
        torrent <- hoistEither =<< lift (T.fromFile file)
        lift $ startDownload client torrent (BS.unpack . idName . tInfoDict $ torrent)
        lift $ trackChanges onActivePeersChanged $ activePeers client
    forever yield

showUsage :: IO ()
showUsage = getProgName >>= \name -> putStrLn $ "Usage: " ++ name ++ " <torrent>"

onActivePeersChanged :: Int -> IO ()
onActivePeersChanged n = putStr "Active peers: " >> print n

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
