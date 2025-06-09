{-# LANGUAGE MultiWayIf #-}

module PcapReplicator.Parser.Unfold (parse, parseU) where

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

parse :: PcapParser
parse readBufferBytes handle = Stream.unfoldrM feed decoder
  where
    decoder = Partial processChunk

    feed (Done leftover packet) = pure (Just (packet, processChunk leftover))
    feed (Partial k) = do
        chunk <- mytake handle readBufferBytes
        if Array.null chunk
            then pure Nothing
            else feed (k chunk)

data Decoder = Done BytesA BytesA | Partial (BytesA -> Decoder)

processChunk :: BytesA -> Decoder
processChunk chunk =
    if len < 16
        then
            askForMore
        else
            let capturedPacketLength' = capturedPacketLength chunk
                off2 = 16 + capturedPacketLength'
             in if
                    | off2 > len -> askForMore
                    | off2 < len ->
                        let (packet, rest) = Array.splitAt off2 chunk
                         in Done rest packet
                    | otherwise -> Done Array.empty chunk
  where
    len = Array.length chunk
    askForMore = Partial $ \newchunk -> processChunk $ chunk <> newchunk
