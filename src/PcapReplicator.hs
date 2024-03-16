module PcapReplicator (BytesA, Client(New), Clients, sendToAll, PcapPacketA, PcapParser, PcapParserName(..), PcapStreamHeaderA, StreamOfBytesA, right, drain) where

import qualified Control.Monad.Catch as E
import Control.Tracer (Tracer)
import Data.Functor.Identity (runIdentity, Identity)
import Data.Text (Text)
import Data.Word (Word8)
import Network.Socket (Socket)
import qualified Streamly.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import System.IO (Handle)

import PcapReplicator.Network


data PcapParserName = Fold | FoldCP | Unfold | ByteString | Array
                    | Parser | ParserMA | ParserCP | ParserChunked | ParserMAChunked | ParserCPChunked
                    deriving (Read, Show)

type BytesA = Array.Array Word8
type PcapStreamHeaderA = BytesA
type PcapPacketA = BytesA
type StreamOfBytesA = Stream.Stream IO BytesA
type PcapParser = Int -> Handle -> StreamOfBytesA

data Client = New !Socket | Old !Socket
type Clients = Stream.Stream Identity Client

sendToAll :: (Stream.MonadAsync m, E.MonadCatch m) => Tracer m (IO Text) -> PcapStreamHeaderA -> PcapPacketA -> Clients -> m Clients
sendToAll tracer header packet clients =
    Stream.fold Fold.toStream (Old <$> Stream.mapMaybeM (uncurry (sendChunkToSocket tracer)) (nextChunk <$> generalizeInner clients))
  where
    nextChunk (New sk) = (header <> packet, sk)
    nextChunk (Old sk) = (packet, sk)
    generalizeInner = Stream.morphInner (return . runIdentity)

right :: Either a b -> b
right (Right r) = r
right _         = error "lala"

drain :: Monad m => Stream.Stream m a -> m ()
drain = Stream.fold Fold.drain
