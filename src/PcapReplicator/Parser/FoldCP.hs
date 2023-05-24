module PcapReplicator.Parser.FoldCP (parse) where

import Data.Word (Word8)
import qualified Streamly.FileSystem.Handle as H
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream
import qualified Streamly.Internal.Data.Array.Stream.Fold.Foreign as ArrayFold
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Prelude as Stream

import PcapReplicator
import PcapReplicator.Parser.Utils


parse :: PcapParser
parse handle =
       ArrayStream.foldArrMany (ArrayFold.fromFold foldPacket)
       $ Stream.unfold H.readChunks handle

foldPacket :: Fold.Fold IO Word8 (Array.Array Word8)
foldPacket = flip Fold.concatMap (Fold.take 16 (Array.writeN 16)) $ \pcap_pkt_hdr ->
    Fold.serialWith (<>) (Fold.fromPure pcap_pkt_hdr)
                         (Fold.take (capturedPacketLength pcap_pkt_hdr) (Array.writeN (capturedPacketLength pcap_pkt_hdr)))
