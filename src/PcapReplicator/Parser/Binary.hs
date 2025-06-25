module PcapReplicator.Parser.Binary (parse) where

import Data.Binary.Get (
    Decoder (..),
    Get,
    getByteString,
    getWord32le,
    lookAhead,
    pushChunk,
    runGetIncremental,
    skip,
 )
import Data.ByteString qualified as BS
import Data.Word (Word32)
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.External.ByteString qualified as Strict

import PcapReplicator

getPacketLength :: Get Word32
getPacketLength = do
    skip 8
    getWord32le

getPacket :: Get BytesA
getPacket = do
    capturedPacketLength' <- lookAhead getPacketLength
    packet <- getByteString (16 + fromIntegral capturedPacketLength')
    pure $! Strict.toArray packet

parse' :: Get BytesA -> PcapParser
parse' parser (ReadBufferBytes readBufferBytes) handle = Stream.unfoldrM feed decoder
  where
    decoder = runGetIncremental parser

    feed (Done leftover _consumed packet) = pure (Just (packet, pushChunk decoder leftover))
    feed (Fail _leftover _consumed _str) = pure Nothing
    feed (Partial k) = do
        chunk <- BS.hGet handle readBufferBytes
        if BS.null chunk
            then feed (k Nothing)
            else feed (k (Just chunk))

parse :: PcapParser
parse = parse' getPacket
