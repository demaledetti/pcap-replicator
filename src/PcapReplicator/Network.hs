{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PcapReplicator.Network (sendChunkToSocket, tcpServer) where

import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word8)
import Network.Socket (getPeerName, Socket, SocketOption(ReuseAddr), close)
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Prelude as Stream


type Chunk = Stream.SerialT IO Word8

tcpServer :: Stream.MonadAsync m => Stream.SerialT m Socket
tcpServer = Stream.mapM connectionMade
          $ TCP.connectionsOnAddrWith sockopts ip port
  where
    connectionMade sk = do
        client <- liftIO $ getPeerName sk
        liftIO $ putStrLn $ "New client socket: " ++ show sk ++ ": " ++ show client
        pure sk
    ip = (0, 0, 0, 0)
    port = 8091
    sockopts = [(ReuseAddr, 1)]

sendChunkToSocket :: Chunk -> Socket -> IO (Maybe Socket)
sendChunkToSocket chunk sk = send `catch` connectionLost
  where
    send = do
        Stream.fold (Socket.write sk) chunk
        pure $ Just sk
    connectionLost (e :: SomeException) = do
        putStrLn $ "Lost client socket: " ++ show sk ++ ": " ++ show e
        close sk
        pure Nothing
