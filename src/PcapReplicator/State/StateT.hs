{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module PcapReplicator.State.StateT (main) where

import Control.Monad (void)
import Control.Monad.Catch qualified as E
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (MonadState, get, modify', put, runStateT)
import Control.Tracer (natTracer)
import Network.Socket (Socket)
import Streamly.Data.Array qualified as Array
import Streamly.Data.Stream.Prelude qualified as Stream

import PcapReplicator
import PcapReplicator.Cli
import PcapReplicator.Log
import PcapReplicator.Network
import PcapReplicator.Parser
import PcapReplicator.Process

data App = App
    { -- static configuration
      config :: !Options
    , -- logging
      tracer :: !TracerIOIOT
    , -- immutable state
      clients :: !Clients
    , lastPcapStreamHeader :: !PcapStreamHeaderA
    }

data Event
    = NewClient !Socket
    | StreamHeader !PcapStreamHeaderA
    | Packet !PcapPacketA
    | SourceFinished
data ProgramState = Run | Done deriving (Eq)

main :: Options -> TracerIOIOT -> IO ()
main config tracer = do
    let app = App config tracer Stream.nil (Array.fromList [])
        runApp =
            drain . Stream.takeWhile (== Run) $
                Stream.mapM onEvent $
                    Stream.parList (Stream.maxBuffer 2) [runServer app, source app]
    void $ runStateT runApp app

onEvent
    :: (MonadState App m, Stream.MonadAsync m, E.MonadCatch m)
    => Event -> m ProgramState
onEvent (Packet p) = do
    app@App{..} <- get
    clients' <- sendToAll (natTracer liftIO tracer) lastPcapStreamHeader p clients
    put $! app{clients = clients'}
    pure Run
onEvent (StreamHeader h) = do
    modify' $ \app -> app{lastPcapStreamHeader = h}
    pure Run
onEvent (NewClient sk) = do
    modify' $ \app@App{..} -> app{clients = New sk `Stream.cons` clients}
    pure Run
onEvent SourceFinished = pure Done

runServer
    :: (Stream.MonadAsync m, E.MonadCatch m) => App -> Stream.Stream m Event
runServer App{..} = NewClient <$> tcpServer (natTracer liftIO tracer)

source :: (Stream.MonadAsync m) => App -> Stream.Stream m Event
source App{..} = Stream.unfoldrM step Nothing
  where
    cmdLine = unwords config.cmd
    PerformanceTunables{..} = config.tunables
    parser = pcapParserImplementation.parser
    step Nothing = do
        -- liftIO $ print "Start process"
        pp@PcapProcess{..} <-
            liftIO $ pcapProcess cmdLine parser bufferBytes readBufferBytes
        pure $ Just (StreamHeader pcapStreamHeader, Just pp)
    step (Just pp@PcapProcess{..}) =
        liftIO (Stream.uncons pcapPacketStream) >>= \case
            Nothing -> if config.once then pure (Just (SourceFinished, Nothing)) else step Nothing
            Just (next, rest) -> pure $ Just (Packet next, Just pp{pcapPacketStream = rest})
