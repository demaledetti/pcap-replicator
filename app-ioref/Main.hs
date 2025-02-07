{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- import Control.Concurrent.Async (race_)
import Control.Monad (forever)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream

import PcapReplicator
import PcapReplicator.Cli (Options (..), parseCli)
import PcapReplicator.Log
import PcapReplicator.Network
import PcapReplicator.Parser (getParser)
import PcapReplicator.Process

data App = App
    { -- static configuration
      config :: !Options
    , -- logging
      tracer :: !TracerIOIOT
    , -- mutable state
      clients :: !(IORef Clients)
    }

main :: IO ()
main = do
    options <- parseCli
    print options
    app <- App options stdoutIOTextTracer <$> newIORef Stream.nil
    drain $
        Stream.take 1 $
            Stream.parSequence (Stream.eager True) $
                Stream.fromList [server app, source app]

-- drain $ Stream.take 1 $ Stream.parEval id $ Stream.fromList [server app, source app]
-- race_ (server app) (source app)

server :: App -> IO ()
server App{..} = smapM_ connectionMade (tcpServer tracer)
  where
    connectionMade sk = atomicModifyIORef' clients (addClient (New sk))
    addClient newClient otherClients = (newClient `Stream.cons` otherClients, ())

source :: App -> IO ()
source App{..} = if config.once then go else forever go
  where
    cmdLine = unwords config.cmd
    parser = getParser config.pcapParserName

    go = do
        PcapProcess{..} <-
            pcapProcess cmdLine parser config.bufferBytes config.readBufferBytes
        smapM_ (send pcapStreamHeader) pcapPacketStream

    send :: PcapStreamHeaderA -> PcapPacketA -> IO ()
    send header packet = do
        connectedClients <- atomicModifyIORef' clients (Stream.nil,)
        remainingClients <- sendToAll tracer header packet connectedClients
        atomicModifyIORef' clients $ \newClients -> (newClients `Stream.append` remainingClients, ())

smapM_ :: (Monad m) => (a -> m b) -> Stream.Stream m a -> m ()
smapM_ f = Stream.fold (Fold.drainMapM f)
