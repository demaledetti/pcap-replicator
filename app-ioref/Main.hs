{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (forever)
import Control.Tracer (Tracer)
import Data.Functor.Contravariant (contramap)
import Data.IORef (atomicModifyIORef', IORef, newIORef)
import Streamly.Prelude ((.:), (|:))
import qualified Streamly.Prelude as Stream

import PcapReplicator
import PcapReplicator.Cli (parseCli, Options(..))
import PcapReplicator.Log (stdoutIOTextTracer)
import PcapReplicator.Network
import PcapReplicator.Process
import PcapReplicator.Parser (getParser)


data App = App {
  -- static configuration
    config :: Options
  -- logging
  , tracer :: Tracer IO TcpTrace
  -- mutable state
  , clients :: IORef Clients
  }

main :: IO ()
main = do
    options <- parseCli
    print options
    app <- App options (contramap tcpTraceLog stdoutIOTextTracer) <$> newIORef Stream.nil
    Stream.drain $ Stream.take 1 $ Stream.fromParallel $ server app |: source app |: Stream.nil

server :: App -> IO ()
server App{..} = Stream.mapM_ connectionMade (tcpServer tracer)
  where
    connectionMade sk = atomicModifyIORef' clients (addClient (New sk))
    addClient newClient otherClients = (newClient .: otherClients, ())

source :: App -> IO ()
source App{..} = if config.once then go else forever go
  where
    cmdLine = unwords config.cmd
    parser = getParser config.pcapParserName

    go = do
        PcapProcess{..} <- pcapProcess cmdLine parser config.bufferBytes
        Stream.mapM_ (send pcapStreamHeader)  pcapPacketStream

    send :: PcapStreamHeaderA -> PcapPacketA -> IO ()
    send header packet = do
        connectedClients <- atomicModifyIORef' clients (Stream.nil,)
        remainingClients <- sendToAll tracer header packet connectedClients
        atomicModifyIORef' clients $ \newClients -> (newClients `Stream.serial` remainingClients, ())
