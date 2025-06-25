{-# LANGUAGE MultiWayIf #-}

module PcapReplicator.Parser.Unfold (parse) where

import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Internal.Data.Array qualified as Array

import PcapReplicator
import PcapReplicator.Parser.Utils

parse :: PcapParser
parse (ReadBufferBytes readBufferBytes) handle = Stream.unfoldrM feed decoder
  where
    decoder = Partial processChunk

    feed (Done packet leftover) = pure (Just (packet, processChunk leftover))
    feed (Partial k) = do
        chunk <- mytake handle readBufferBytes
        if Array.null chunk
            then pure Nothing
            else feed (k chunk)

data Decoder = Done !BytesA !BytesA | Partial !(BytesA -> Decoder)

processChunk :: BytesA -> Decoder
processChunk chunk =
    if len < 16
        then
            askForMore
        else
            let !capturedPacketLength' = capturedPacketLength chunk
                !off2 = 16 + capturedPacketLength'
             in if
                    | off2 < len -> uncurry Done (Array.splitAt off2 chunk)
                    | off2 > len -> askForMore
                    | otherwise -> Done chunk Array.empty
  where
    len = Array.length chunk
    askForMore = Partial (\newchunk -> processChunk $! chunk <> newchunk)
