{-# LANGUAGE RecordWildCards #-}
module PcapReplicator.Process (PcapProcess(..), pcapProcess) where

import qualified Data.ByteString as BS
import qualified Streaming.Prelude as Streaming
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Prelude as Stream
import System.Process (createProcess, CreateProcess(..), ProcessHandle, shell, StdStream(..))

import PcapReplicator
import PcapReplicator.Parser


pcap2stream :: Packet -> PcapPacket
pcap2stream Packet{..} = Stream.unfold Strict.read header `Stream.serial` Stream.unfold Strict.read bytes

data PcapProcess = PcapProcess {
      pcapStreamHeader :: PcapStreamHeader
    , pcapPacketStream :: Stream.SerialT IO PcapPacket
    , processHandle :: ProcessHandle
    }

pcapProcess :: String -> IO PcapProcess
pcapProcess cmdLine = do
      (_, Just hout, _, processHandle) <- createProcess (shell cmdLine){ std_out = CreatePipe }
      pcapStreamHeader <- Stream.fromList . BS.unpack <$> BS.hGet hout 24
      let pcapPacketStream = Stream.map pcap2stream $ Stream.unfoldrM Streaming.uncons (offline hout)
      return PcapProcess{..}
