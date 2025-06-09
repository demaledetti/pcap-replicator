module PcapReplicator.Parser.Streaming (parse) where

import Control.Monad (void)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Streaming qualified as AS
import Data.Attoparsec.Combinator qualified as AC
import Streaming.ByteString qualified as Q
import Streaming.Prelude qualified as Streaming
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.External.ByteString qualified as Strict

import PcapReplicator
import PcapReplicator.Parser.Utils

parse :: PcapParser
parse = parse' packet

parse' :: A.Parser PcapPacketA -> PcapParser
parse' parser readBufferBytes handle = Stream.unfoldrM Streaming.uncons streamFromHandle
  where
    -- The following bits are humbly adapted from
    -- <https://hackage.haskell.org/package/streaming-pcap streaming-pcap> by Colin Woodbury

    streamFromHandle = void . AS.parsed parser $ Q.hGetContentsN readBufferBytes handle

packetLength :: A.Parser Int
packetLength = capturedPacketLengthBS <$> A.take 12

packet :: A.Parser PcapPacketA
packet = do
    capturedPacketLength' <- AC.lookAhead packetLength
    packet' <- A.take (16 + capturedPacketLength')
    pure $! Strict.toArray packet'
