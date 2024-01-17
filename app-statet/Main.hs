{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Control.Monad.Catch as E
import Control.Tracer (natTracer)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState, get, modify, put, runStateT)
import Network.Socket (Socket)
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Stream.Prelude as Stream

import PcapReplicator
import PcapReplicator.Cli (parseCli, Options(..))
import PcapReplicator.Log
import PcapReplicator.Network
import PcapReplicator.Process
import PcapReplicator.Parser (getParser)


data App = App {
  -- static configuration
    config :: !Options
  -- logging
  , tracer :: !TracerIOIOT
  -- immutable state
  , clients :: !Clients
  , lastPcapStreamHeader :: !PcapStreamHeaderA
  }

data Event = NewClient !Socket | StreamHeader !PcapStreamHeaderA | Packet !PcapPacketA | SourceFinished
data ProgramState = Run | Done deriving Eq

main :: IO ()
main = do
    options <- parseCli
    print options

    let app = App options stdoutIOTextTracer Stream.nil (Array.fromList [])
        runApp = drain . Stream.takeWhile (== Run)
               $ Stream.mapM onEvent
               $ Stream.parList (Stream.maxBuffer 2) [server app, source app]
    void $ runStateT runApp app

onEvent :: (MonadState App m, Stream.MonadAsync m, E.MonadCatch m) => Event -> m ProgramState
onEvent (Packet p) = do
    app@App{..} <- get
    clients' <- sendToAll (natTracer liftIO tracer) lastPcapStreamHeader p clients
    put app { clients = clients' }
    pure Run
onEvent (StreamHeader h) = do
    modify $ \app -> app { lastPcapStreamHeader = h }
    pure Run
onEvent (NewClient sk) = do
    modify $ \app@App{..} -> app { clients = New sk `Stream.cons` clients }
    pure Run
onEvent SourceFinished = pure Done

server :: (Stream.MonadAsync m, E.MonadCatch m) => App -> Stream.Stream m Event
server App{..} = NewClient <$> tcpServer (natTracer liftIO tracer)

source :: Stream.MonadAsync m => App -> Stream.Stream m Event
source App{..} = Stream.unfoldrM step Nothing
    where
      cmdLine = unwords config.cmd
      parser = getParser config.pcapParserName
      step Nothing = do
          -- liftIO $ print "Start process"
          pp@PcapProcess{..} <- liftIO $ pcapProcess cmdLine parser config.bufferBytes config.readBufferBytes
          pure $ Just (StreamHeader pcapStreamHeader, Just pp)
      step (Just pp@PcapProcess{..}) = liftIO (Stream.uncons pcapPacketStream) >>= \case
          Nothing -> if config.once then pure (Just (SourceFinished, Nothing)) else step Nothing
          Just (next, rest) -> pure $ Just (Packet next, Just pp { pcapPacketStream = rest })
