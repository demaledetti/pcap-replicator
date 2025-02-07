module PcapReplicator.Parser.FoldCP (parseFCP) where

import Data.Word (Word8)
import Streamly.Data.StreamK qualified as StreamK
import Streamly.FileSystem.Handle qualified as H
import Streamly.Internal.Data.Array qualified as Array
import Streamly.Internal.Data.Array.Stream qualified as ArrayStream
import Streamly.Internal.Data.Fold.Chunked qualified as ChunkFold

import PcapReplicator
import PcapReplicator.Parser.Utils

-- parse :: PcapParser
-- parse handle =
--        right <$> StreamK.toStream ( ArrayStream.runArrayFoldMany (ChunkFold.fromFold foldPacket)
--        $ StreamK.fromStream $ Stream.unfold H.chunkReader handle )

parseFCP :: PcapParser
parseFCP readBufferBytes handle =
    right
        <$> StreamK.toStream
            ( ArrayStream.runArrayFoldMany foldPacket $
                StreamK.fromStream $
                    H.readChunksWith readBufferBytes handle
            )

foldPacket :: ChunkFold.ChunkFold IO Word8 (Array.Array Word8)
foldPacket = flip
    ChunkFold.concatMap
    (ChunkFold.take 16 (ChunkFold.fromFold (Array.writeN 16)))
    $ \pcap_pkt_hdr ->
        ChunkFold.splitWith
            (<>)
            (ChunkFold.fromPure pcap_pkt_hdr)
            ( ChunkFold.take
                (capturedPacketLength pcap_pkt_hdr)
                (ChunkFold.fromFold (Array.writeN (capturedPacketLength pcap_pkt_hdr)))
            )

-- foldPacket :: Fold.Fold IO Word8 (Array.Array Word8)
-- foldPacket = flip Fold.concatMap (Fold.take 16 (Array.writeN 16)) $ \pcap_pkt_hdr ->
--     Fold.splitWith (<>) (Fold.fromPure pcap_pkt_hdr)
--                          (Fold.take (capturedPacketLength pcap_pkt_hdr) (Array.writeN (capturedPacketLength pcap_pkt_hdr)))
