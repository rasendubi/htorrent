{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Torrent as T
import qualified Data.ByteString.Char8 as BS
import Network.BitTorrent.Tracker
import Network.HTTP
import Control.Monad
import System.Exit
import System.Environment

import Control.Monad.Trans.Either
import Control.Monad.Trans

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showUsage >> exitFailure
        _ -> return ()
    forM_ args $ \file -> runEitherT $ do
        torrent <- hoistEither =<< lift (T.fromFile file)
        lputStrLn $ file ++ ": " ++ show torrent
        let uri = announceURI torrent peerId
        lputStrLn $ "\nAnnouncing to " ++ uri
        rsp <- bimapEitherT show id (hoistEither =<< lift (simpleHTTP $ getRequest uri))
        lputStrLn "Got responce:"
        lprint rsp
        lprint $ rspBody rsp
        let response = decodeResponse . BS.pack $ rspBody rsp
        lprint response

lprint :: (Show a) => a -> EitherT String IO ()
lprint = lift . print

lputStrLn :: String -> EitherT String IO ()
lputStrLn = lift . putStrLn

showUsage :: IO ()
showUsage = getProgName >>= \name -> putStrLn $ "Usage: " ++ name ++ " <torrent>"

peerId :: BS.ByteString
peerId = "HASKELL7TORRENT5YEAH"
