module PcapReplicator (Client(New), Clients, sendToAll, PcapPacket, PcapStreamHeader) where

import Data.Word (Word8)
import Network.Socket (Socket)
import Streamly.Internal.Data.Stream.IsStream.Eliminate (toStream)
import qualified Streamly.Prelude as Stream

import PcapReplicator.Network


type StreamOfBytes = Stream.SerialT IO Word8
type PcapStreamHeader = StreamOfBytes
type PcapPacket = StreamOfBytes

data Client = New Socket | Old Socket
type Clients = Stream.SerialT IO Client

sendToAll :: PcapStreamHeader -> PcapPacket -> Clients -> IO Clients
sendToAll header packet clients =
      toStream
    $ Stream.fromAsync
    $ Stream.map Old
    $ Stream.mapMaybeM (uncurry sendChunkToSocket)
    $ Stream.map nextChunk
    $ Stream.fromSerial clients
  where
    nextChunk (New sk) = (header `Stream.serial` packet, sk)
    nextChunk (Old sk) = (packet, sk)
