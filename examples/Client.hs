{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Torrent as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Network.BitTorrent.Tracker
import Network.BitTorrent.Peer
import Network.HTTP hiding (Response)
import Network.Socket.ByteString (recv)
import Control.Monad
import System.Exit
import System.Environment
import Data.Binary
import Control.Applicative

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
        lputStr "My handshake is: "
        lprint $ handshake peerId (getInfoHash torrent)
        lputStrLn $ file ++ ": " ++ show torrent
        let uri = announceURI torrent peerId
        lputStrLn $ "\nAnnouncing to " ++ uri
        rsp <- bimapEitherT show id (hoistEither =<< lift (simpleHTTP $ getRequest uri))
        lputStrLn "Got responce:"
        lprint rsp
        lprint $ rspBody rsp
        response <- hoistEither $ decodeResponse . BS.pack $ rspBody rsp
        lprint response
        let (Response _ peers) = response
        lift $ forM_ peers (tryPeer torrent)

tryPeer :: Torrent -> Peer -> IO ()
tryPeer torrent peer = do
    putStrLn $ "\nConnecting to " ++ show peer
    conn_ <- connectToPeer peer
    case conn_ of
        Left err -> putStrLn $ "Error: " ++ err
        Right conn -> do
            sendHandshake peerId (getInfoHash torrent) conn
            response <- decode . BL.fromStrict <$> recv conn 68 :: IO Handshake
            putStr "Got response: "
            print response
            sendMessage Interested conn
            print =<< socketToMessages conn

lprint :: (Show a) => a -> EitherT String IO ()
lprint = lift . print

lputStr :: String -> EitherT String IO ()
lputStr = lift . putStr

lputStrLn :: String -> EitherT String IO ()
lputStrLn = lift . putStrLn

showUsage :: IO ()
showUsage = getProgName >>= \name -> putStrLn $ "Usage: " ++ name ++ " <torrent>"

peerId :: BS.ByteString
peerId = "HASKELL7TORRENT5YEAH"
