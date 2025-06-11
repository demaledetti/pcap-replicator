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
import PcapReplicator.Cli
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

bestBufferSizeForIORefImpl :: Int
bestBufferSizeForIORefImpl = 64 * 1024

main :: IO ()
main = do
    options <- parseCli bestBufferSizeForIORefImpl
    print options
    app <- App options stdoutIOTextTracer <$> newIORef Stream.nil
    drain $
        Stream.take 1 $
            Stream.parSequence (Stream.eager True) $
                Stream.fromList [runServer app, source app]

-- drain $ Stream.take 1 $ Stream.parEval id $ Stream.fromList [server app, source app]
-- race_ (server app) (source app)

runServer :: App -> IO ()
runServer App{..} = smapM_ connectionMade (tcpServer tracer)
  where
    connectionMade sk = atomicModifyIORef' clients (addClient (New sk))
    addClient newClient otherClients = (newClient `Stream.cons` otherClients, ())

source :: App -> IO ()
source App{..} = if config.once then once else forever go
  where
    cmdLine = unwords config.cmd
    PerformanceTunables{..} = config.tunables
    parser = getParser pcapParserName

    go = do
        PcapProcess{..} <-
            pcapProcess cmdLine parser bufferBytes readBufferBytes
        smapM_ (send pcapStreamHeader) pcapPacketStream
    --     Stream.fold Fold.length $ Stream.trace (send pcapStreamHeader) pcapPacketStream

    once = go
    -- once = do
    --   streamItems <- go
    --   putStrLn $ "Stream items: " <> show streamItems

    send :: PcapStreamHeaderA -> PcapPacketA -> IO ()
    send header packet = do
        connectedClients <- atomicModifyIORef' clients (Stream.nil,)
        remainingClients <- sendToAll tracer header packet connectedClients
        atomicModifyIORef' clients $ \newClients -> (newClients `Stream.append` remainingClients, ())

-- this is called smapM_ instead of mapM_ to avoid conflict with the prelude
smapM_ :: (Monad m) => (a -> m b) -> Stream.Stream m a -> m ()
smapM_ f = Stream.fold (Fold.drainMapM f)
