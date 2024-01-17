module PcapReplicator.Parser.Unfold (parseU) where

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Data.Stream.Prelude as Stream

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
