module PcapReplicator.Parser.Unfold (parse) where

import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Prelude as Stream

import PcapReplicator
import PcapReplicator.Parser.Utils


parse :: PcapParser
parse = Stream.unfoldrM go
  where
    go rest = do
        pcapHeader <- mytake rest 16
        if Array.null pcapHeader
            then pure Nothing
            else do
                pcapPacket <- mytake rest $ capturedPacketLength pcapHeader
                pure $ Just (pcapHeader <> pcapPacket, rest)
