module PcapReplicator (BytesA, Client(New), Clients, sendToAll, PcapPacketA, PcapParser, PcapParserName(..), PcapStreamHeaderA, StreamOfBytesA) where

import qualified Control.Monad.Catch as E
import Control.Tracer (Tracer)
import Data.Functor.Identity (Identity)
import Data.Word (Word8)
import Network.Socket (Socket)
import qualified Streamly.Data.Array.Foreign as Array
import Streamly.Internal.Data.Stream.IsStream.Eliminate (toStream)
import Streamly.Internal.Data.Stream.IsStream.Lift (generally)
import qualified Streamly.Prelude as Stream
import System.IO (Handle)

import PcapReplicator.Network


data PcapParserName = Fold | FoldCP | Unfold | ByteString | Array | Parser | ParserCP
                    deriving (Read, Show)

type BytesA = Array.Array Word8
type PcapStreamHeaderA = BytesA
type PcapPacketA = BytesA
type StreamOfBytesA = Stream.SerialT IO BytesA
type PcapParser = Handle -> StreamOfBytesA

data Client = New Socket | Old Socket
type Clients = Stream.SerialT Identity Client

sendToAll :: (Stream.MonadAsync m, E.MonadCatch m) => Tracer m TcpTrace -> PcapStreamHeaderA -> PcapPacketA -> Clients -> m Clients
sendToAll tracer header packet clients =
      toStream
    $ Stream.fromAsync
    $ Stream.map Old
    $ Stream.mapMaybeM (uncurry (sendChunkToSocket tracer))
    $ Stream.map nextChunk
    $ Stream.fromSerial
    $ generally clients
  where
    nextChunk (New sk) = (header <> packet, sk)
    nextChunk (Old sk) = (packet, sk)
