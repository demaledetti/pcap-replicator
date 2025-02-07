module PcapReplicator.Parser.Streaming (parseA, parseBS) where

import Control.Monad (void)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Streaming qualified as AS
import Streaming.ByteString qualified as Q
import Streaming.Prelude qualified as Streaming
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.External.ByteString qualified as Strict

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

packetA :: A.Parser PcapPacketA
packetA = do
    hdr <- Strict.toArray <$> A.take 16
    bts <- Strict.toArray <$> A.take (capturedPacketLength hdr)
    pure $! hdr <> bts
