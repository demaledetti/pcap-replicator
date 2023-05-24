{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Control.Monad.Catch as E
import Control.Tracer (Tracer)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState, get, modify, put, runStateT)
import Data.Functor.Contravariant (contramap)
import Debug.Trace (trace)
import Network.Socket (Socket)
import qualified Streamly.Data.Array.Foreign as Array
import Streamly.Prelude ((.:))
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
  -- immutable state
  , clients :: Clients
  , lastPcapStreamHeader :: PcapStreamHeaderA
  }

data Event = NewClient Socket | StreamHeader PcapStreamHeaderA | Packet PcapPacketA | SourceFinished
data ProgramState = Run | Done deriving Eq

main :: IO ()
main = do
    options <- parseCli
    print options

    let app = App options Stream.nil (Array.fromList [])
        tracer = contramap tcpTraceLog stdoutIOTextTracer
        runApp = Stream.drainWhile (== Run)
               $ Stream.mapM (onEvent tracer)
               $ server tracer `Stream.parallel` source app
    void $ runStateT runApp app

onEvent :: (MonadState App m, Stream.MonadAsync m, E.MonadCatch m) => Tracer m TcpTrace -> Event -> m ProgramState
onEvent tracer (Packet p) = do
    app@App{..} <- get
    clients' <- sendToAll tracer lastPcapStreamHeader p clients
    put app { clients = clients' }
    pure Run
onEvent _ (StreamHeader h) = do
    modify $ \app -> app { lastPcapStreamHeader = h }
    pure Run
onEvent _ (NewClient sk) = do
    modify $ \app@App{..} -> app { clients = New sk .: clients }
    pure Run
onEvent _ SourceFinished = pure Done

server :: (Stream.MonadAsync m, E.MonadCatch m) => Tracer m TcpTrace -> Stream.SerialT m Event
server tracer = tryAgain
  where
    handler (e :: E.SomeException) = trace ("Server failed: " ++ show e) tryAgain
    tryAgain = Stream.handle handler socketStream
    socketStream = NewClient <$> tcpServer tracer

source :: Stream.MonadAsync m => App -> Stream.SerialT m Event
source App{..} = Stream.unfoldrM step Nothing
    where
      cmdLine = unwords config.cmd
      parser = getParser config.pcapParserName
      step Nothing = do
          pp@PcapProcess{..} <- liftIO $ pcapProcess cmdLine parser config.bufferBytes
          pure $ Just (StreamHeader pcapStreamHeader, Just pp)
      step (Just pp@PcapProcess{..}) = liftIO (Stream.uncons pcapPacketStream) >>= \case
          Nothing -> if config.once then pure (Just (SourceFinished, Nothing)) else step Nothing
          Just (next, rest) -> pure $ Just (Packet next, Just pp { pcapPacketStream = rest })
