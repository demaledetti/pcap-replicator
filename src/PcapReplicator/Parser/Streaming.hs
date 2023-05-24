{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module PcapReplicator.Parser.Streaming (parse, parse2) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Streaming as AS
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Word (Word32, Word8)
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as Streaming
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Prelude as Stream
import System.IO (Handle)

import PcapReplicator
import PcapReplicator.Parser.Utils


pcap2stream :: Packet -> PcapPacketA
pcap2stream Packet{..} = Strict.toArray $ header <> bytes

parse :: Handle -> Stream.SerialT IO (Array.Array Word8)
parse handle = Stream.map pcap2stream $ Stream.unfoldrM Streaming.uncons (offline handle)

parse2 :: Handle -> Stream.SerialT IO (Array.Array Word8)
parse2 handle = Stream.unfoldrM Streaming.uncons (offline2 handle)

-- The following bits are humbly adapted from
-- <https://hackage.haskell.org/package/streaming-pcap streaming-pcap> by Colin Woodbury

data Packet = Packet { header :: BS.ByteString, bytes :: BS.ByteString }

packetAP :: A.Parser PcapPacketA
packetAP = do
  hdr <- Strict.toArray <$> A.take 16
  bts <- Strict.toArray <$> A.take (capturedPacketLength hdr)
  pure $ hdr <> bts

packetP :: A.Parser Packet
packetP = do
  hdr <- A.take 16
  bts <- A.take . fromIntegral . four . BS.take 4 $ BS.drop 8 hdr
  pure $ Packet hdr bts

four :: BS.ByteString -> Word32
four = Get.runGet Get.getWord32le . LB.fromStrict
{-# INLINE four #-}

offline :: MonadIO m => Handle -> Streaming.Stream (Streaming.Of Packet) m ()
offline = void . AS.parsed packetP . Q.fromHandle

offline2 :: MonadIO m => Handle -> Streaming.Stream (Streaming.Of PcapPacketA) m ()
offline2 = void . AS.parsed packetAP . Q.fromHandle
