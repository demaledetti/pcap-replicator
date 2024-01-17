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


data PcapParserName = Dummy
                  --  ^ the Dummy value here is a workaround for a GHC bug triggered by -O2
                  -- If we remove it then the getParser function always returns the same
                  -- parser (the 7th), regardless of the name passed to it.
                  -- If the Dummy value appears from the 7th position onwards, then the getParser function
                  -- behaves as if it got passed that value (and thus blows up), regardless of the name
                  -- passed to it.
                  -- Commenting cases in the getParser function makes some cases work and some cases blow up.
                  -- The bug can also be avoided by introducing some Debug.Trace.trace calls in getParser,
                  -- which makes it a Heisenbug.
                    | Fold | FoldCP | Unfold | ByteString | Array
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
