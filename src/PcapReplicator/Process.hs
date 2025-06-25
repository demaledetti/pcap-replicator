{-# LANGUAGE RecordWildCards #-}

module PcapReplicator.Process (PcapProcess (..), pcapProcess) where

import Streamly.Internal.Data.Array qualified as Array

-- import qualified Streamly.Internal.Data.Fold as Fold
import Streamly.Data.Stream.Prelude qualified as Stream
import System.IO (hSetBinaryMode)
import System.Process (
    CreateProcess (..),
    StdStream (..),
    createProcess,
    shell,
    waitForProcess,
 )

import PcapReplicator
import PcapReplicator.Parser.Utils

data PcapProcess = PcapProcess
    { pcapStreamHeader :: !PcapStreamHeaderA
    , pcapPacketStream :: !StreamOfBytesA
    }

pcapProcess
    :: String -> PcapParser -> WriteBufferBytes -> ReadBufferBytes -> IO PcapProcess
pcapProcess cmdLine parser (WriteBufferBytes bufferBytes) readBufferBytes = do
    (_, Just stdoutHandle, _, processHandle) <-
        createProcess (shell cmdLine){std_out = CreatePipe}
    hSetBinaryMode stdoutHandle True
    pcapStreamHeader <- mytake stdoutHandle 24
    let pcapPacketStream' = buffer $ parser readBufferBytes stdoutHandle
        pcapPacketStream = Stream.after (waitForProcess processHandle) pcapPacketStream'
    return PcapProcess{..}
  where
    buffer s = if bufferBytes > 0 then Array.compactLE bufferBytes s else s

-- buffering by number of elements (packets)
-- Stream.mapM ArrayStream.toArray $ Stream.chunksOf 20 Fold.toStream s
