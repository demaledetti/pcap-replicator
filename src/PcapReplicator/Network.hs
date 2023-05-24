{-# LANGUAGE OverloadedStrings #-}
module PcapReplicator.Network (sendChunkToSocket, tcpServer, TcpTrace, tcpTraceLog) where

import Control.Exception (catch, IOException)
import qualified Control.Monad.Catch as E
import Control.Monad.IO.Class (liftIO)
import Control.Tracer (Tracer, traceWith)
import qualified Data.Text as T
import Data.Word (Word8)
import Network.Simple.TCP (closeSock)
import Network.Socket (getPeerName, Socket, SocketOption(..))
import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Prelude as Stream

import PcapReplicator.Log (tshow)


type Chunk = Array.Array Word8

data TcpTraceEventType = TcpConnectionMade | TcpConnectionLost IOException
data TcpTrace = TcpTrace TcpTraceEventType Socket

tcpTraceLog :: TcpTrace -> IO T.Text
tcpTraceLog (TcpTrace TcpConnectionMade sk) = tcpTraceLog' "New" sk
tcpTraceLog (TcpTrace (TcpConnectionLost e) sk) = tcpTraceLog' "Lost" sk >>= \msg -> pure $ msg <> ": " <> tshow e

tcpTraceLog' :: T.Text -> Socket -> IO T.Text
tcpTraceLog' msg sk = (tshow <$> getPeerName sk) `catch` unknownClient >>= \client -> pure $ msg <> " client socket: " <> tshow sk <> ": " <> client

unknownClient :: IOException -> IO T.Text
unknownClient = const $ pure "unknown"

tcpServer :: (Stream.MonadAsync m) => Tracer m TcpTrace -> Stream.SerialT m Socket
tcpServer tracer =
      Stream.trace (traceWith tracer . TcpTrace TcpConnectionMade)
    $ TCP.connectionsOnAddrWith sockopts ip port
  where
    ip = (0, 0, 0, 0)
    port = 8091
    sockopts = [(ReuseAddr, 1)]

sendChunkToSocket :: (Stream.MonadAsync m, E.MonadCatch m) => Tracer m TcpTrace -> Chunk -> Socket -> m (Maybe Socket)
sendChunkToSocket tracer chunk sk = send `E.catchIOError` connectionLost
  where
    send = do
        liftIO $ Socket.writeChunk sk chunk
        pure $ Just sk
    connectionLost e = do
        traceWith tracer $ TcpTrace (TcpConnectionLost e) sk
        closeSock sk
        pure Nothing
