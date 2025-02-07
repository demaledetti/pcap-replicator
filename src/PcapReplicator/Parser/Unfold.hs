module PcapReplicator.Parser.Unfold (parseU) where

import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Internal.Data.Array qualified as Array

import PcapReplicator
import PcapReplicator.Parser.Utils

parseU :: PcapParser
parseU _readBufferBytes = Stream.unfoldrM go
  where
    go rest = do
        pcapHeader <- mytake rest 16
        if Array.null pcapHeader
            then pure Nothing
            else do
                pcapPacket <- mytake rest $ capturedPacketLength pcapHeader
                pure $ Just (pcapHeader <> pcapPacket, rest)
