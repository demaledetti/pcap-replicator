{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (forever, void)
import Data.IORef (atomicModifyIORef', IORef, newIORef)
import Streamly.Prelude ((.:), (|:))
import qualified Streamly.Prelude as Stream
import System.Environment (getArgs)
import System.Process (waitForProcess)

import PcapReplicator
import PcapReplicator.Network
import PcapReplicator.Process


data App = App {
    clients :: IORef Clients
  , cmdLine :: String
  , lastPcapStreamHeader :: PcapStreamHeader
  }

main :: IO ()
main = do
    app <- App <$> newIORef Stream.nil <*> (unwords <$> getArgs) <*> pure Stream.nil
    Stream.drain $ Stream.fromParallel $ server app |: source app |: Stream.nil

server :: App -> IO ()
server App{..} = Stream.mapM_ connectionMade tcpServer
  where
    connectionMade sk = atomicModifyIORef' clients (addClient (New sk))
    addClient newClient otherClients = (newClient .: otherClients, ())

source :: App -> IO ()
source app@App{..} = forever $ do
    PcapProcess{..} <- pcapProcess cmdLine
    let app' = app { lastPcapStreamHeader = pcapStreamHeader }
    Stream.mapM_ (sendToAll' app') pcapPacketStream
    void $ waitForProcess processHandle

sendToAll' :: App -> PcapPacket -> IO ()
sendToAll' App{..} packet = do
    connectedClients <- atomicModifyIORef' clients (Stream.nil,)
    remainingClients <- sendToAll lastPcapStreamHeader packet connectedClients
    atomicModifyIORef' clients $ \newClients -> (newClients `Stream.serial` remainingClients, ())
