{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Torrent as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
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
        lputStr "All requests are: "
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
            mapM_ (handleMessage conn torrent) =<< socketToMessages conn

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
peerId = "HASKELL7TORRENT5YEAH"
