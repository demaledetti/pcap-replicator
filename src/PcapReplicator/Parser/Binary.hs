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
parse' parser readBufferBytes handle = Stream.unfoldrM go (decoder, handle)
  where
    -- go (d, h) = feed d h
    go = uncurry feed

    decoder = runGetIncremental parser

    feed (Done leftover _consumed packet) h = pure (Just (packet, (pushChunk decoder leftover, h)))
    feed (Fail _leftover _consumed _str) _ = pure Nothing
    feed (Partial k) h = do
        chunk <- BS.hGet h readBufferBytes
        if BS.null chunk
            then feed (k Nothing) h
            else feed (k (Just chunk)) h

parse :: PcapParser
parse = parse' getPacket
