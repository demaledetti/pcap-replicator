{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (MonadState, get, modify, put, runStateT)
import Network.Socket (Socket)
import Streamly.Prelude ((.:))
import qualified Streamly.Prelude as Stream
import System.Environment (getArgs)
import System.Process (waitForProcess)

import PcapReplicator
import PcapReplicator.Network
import PcapReplicator.Process


data App = App {
    clients :: Clients
  , lastPcapStreamHeader :: PcapStreamHeader
  }

data Event = NewClient Socket | StreamHeader PcapStreamHeader | Packet PcapPacket

main :: IO ()
main = do
    cmdLine <- unwords <$> getArgs
    let app = App Stream.nil Stream.nil
        runApp = Stream.mapM_ onEvent
               $ server `Stream.parallel` source cmdLine
    void $ runStateT runApp app

onEvent :: (MonadState App m, MonadIO m) => Event -> m ()
onEvent (Packet p) = do
    app@App{..} <- get
    clients' <- liftIO $ sendToAll lastPcapStreamHeader p clients
    put app { clients = clients' }
onEvent (StreamHeader h) = modify $ \app -> app { lastPcapStreamHeader = h }
onEvent (NewClient sk) = modify $ \app@App{..} -> app { clients = New sk .: clients }

server :: Stream.MonadAsync m => Stream.SerialT m Event
server = NewClient <$> tcpServer

source :: Stream.MonadAsync m => String -> Stream.SerialT m Event
source cmdLine = Stream.unfoldrM step Nothing
    where
        step Nothing = do
            pp@PcapProcess{..} <- liftIO $ pcapProcess cmdLine
            pure $ Just (StreamHeader pcapStreamHeader, Just pp)
        step (Just pp@PcapProcess{..}) = liftIO (Stream.uncons pcapPacketStream) >>= \case
            Nothing -> liftIO (waitForProcess processHandle) >> step Nothing
            Just (next, rest) -> pure $ Just (Packet next, Just pp { pcapPacketStream = rest })
