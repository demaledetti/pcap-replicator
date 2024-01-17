module PcapReplicator.Parser.Streaming (parseA, parseBS) where

import Control.Monad (void)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Streaming as AS
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as Streaming
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Data.Stream.Prelude as Stream

import PcapReplicator
import PcapReplicator.Parser.Utils


parseBS :: PcapParser
parseBS = parse' packetBS

parseA :: PcapParser
parseA = parse' packetA

parse' :: A.Parser PcapPacketA -> PcapParser
parse' parser readBufferBytes handle = Stream.unfoldrM Streaming.uncons streamFromHandle
  where

-- The following bits are humbly adapted from
-- <https://hackage.haskell.org/package/streaming-pcap streaming-pcap> by Colin Woodbury

    streamFromHandle = void . AS.parsed parser $ Q.hGetContentsN readBufferBytes handle

packetBS :: A.Parser PcapPacketA
packetBS = do
  hdr <- A.take 16
  bts <- A.take $ capturedPacketLengthBS hdr
  pure $! Strict.toArray $! hdr <> bts

packetA:: A.Parser PcapPacketA
packetA = do
  hdr <- Strict.toArray <$> A.take 16
  bts <- Strict.toArray <$> A.take (capturedPacketLength hdr)
  pure $! hdr <> bts
