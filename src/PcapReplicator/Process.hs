{-# LANGUAGE RecordWildCards #-}
module PcapReplicator.Process (PcapProcess(..), pcapProcess) where

import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream
--import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Prelude as Stream
import System.Process (createProcess, CreateProcess(..), shell, StdStream(..), waitForProcess)

import PcapReplicator
import PcapReplicator.Parser.Utils


data PcapProcess = PcapProcess {
      pcapStreamHeader :: PcapStreamHeaderA
    , pcapPacketStream :: StreamOfBytesA
    }

pcapProcess :: String -> PcapParser -> Int -> IO PcapProcess
pcapProcess cmdLine parser bufferBytes = do
      (_, Just stdoutHandle, _, processHandle) <- createProcess (shell cmdLine){ std_out = CreatePipe }
      pcapStreamHeader <- mytake stdoutHandle 24
      let pcapPacketStream' = buffer $ parser stdoutHandle
          pcapPacketStream = Stream.after (waitForProcess processHandle) pcapPacketStream'
      return PcapProcess{..}
  where
    buffer s = if bufferBytes > 0 then ArrayStream.compact bufferBytes s else s
    -- buffering by number of elements (packets)
    -- Stream.mapM ArrayStream.toArray $ Stream.chunksOf 20 Fold.toStream s
