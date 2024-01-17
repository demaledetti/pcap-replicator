module PcapReplicator.Parser.Parser (parseP, parseCP, parseMA, parsePChunked, parseCPChunked, parseMAChunked) where

import Control.Monad.IO.Class (liftIO)
import Data.Word (Word8)
import qualified Streamly.Data.MutArray as MArray
import qualified Streamly.FileSystem.Handle as H
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.ParserK as ParserK
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.StreamK as StreamK

import PcapReplicator
import PcapReplicator.Parser.Utils


type StreamOfBytesParser = Parser.Parser Word8 IO (Array.Array Word8)


parseChunked' :: StreamOfBytesParser -> PcapParser
parseChunked' parser readBufferBytes handle =
    Stream.unfoldrM step (StreamK.fromStream $ H.readChunksWith readBufferBytes handle)
    where
        step s = do
            (n, r) <- StreamK.parseBreakChunks (ParserK.adaptC parser) s
            case n of
                Left _ -> pure Nothing
                Right p -> pure $ Just (p, r)
{-# INLINE parseChunked' #-}

-- removing either the above INLINE pragma or the below
-- eta-expansion (or both) kills performance by 50-100%

parsePChunked :: PcapParser
parsePChunked a b = parseChunked' parsePacket a b

parseMAChunked :: PcapParser
parseMAChunked a b = parseChunked' parsePacketMA a b

parseCPChunked :: PcapParser
parseCPChunked a b = parseChunked' parsePacketCP a b



parse' :: StreamOfBytesParser -> PcapParser
parse' parser readBufferBytes handle =
    right <$> Stream.parseMany parser (H.readWith readBufferBytes handle)

-- removing the below eta-expansion kills performance by ~25%

parseP :: PcapParser
parseP a b = parse' parsePacket a b

parseMA :: PcapParser
parseMA a b = parse' parsePacketMA a b

parseCP :: PcapParser
parseCP a b = parse' parsePacketCP a b

parsePacket :: StreamOfBytesParser
parsePacket = do
    pcap_pkt_hdr <- parseFixedSize 16
    pcap_pkt_data <- parseFixedSize (capturedPacketLength pcap_pkt_hdr)
    pure $ pcap_pkt_hdr <> pcap_pkt_data

parsePacketMA :: StreamOfBytesParser
parsePacketMA = do
    pcap_pkt_hdr <- Parser.takeEQ 16 (MArray.writeAppend (MArray.pinnedNew 2048))
    l <- liftIO $ capturedPacketLengthMA pcap_pkt_hdr
    Array.unsafeFreeze <$> Parser.takeEQ l (MArray.writeAppendNUnsafe l (pure pcap_pkt_hdr))

parsePacketCP :: StreamOfBytesParser
parsePacketCP = parseFixedSizeAndContinueWith 16 $ \pcap_pkt_hdr ->
    Parser.splitWith (<>) (Parser.fromPure pcap_pkt_hdr)
                           (parseFixedSize (capturedPacketLength pcap_pkt_hdr))

parseFixedSize :: Int -> StreamOfBytesParser
parseFixedSize size = Parser.takeEQ size (Array.writeN size)
{-# INLINE parseFixedSize #-}

parseFixedSizeAndContinueWith :: Int -> (Array.Array Word8 -> StreamOfBytesParser) -> StreamOfBytesParser
parseFixedSizeAndContinueWith size = flip Parser.concatMap (parseFixedSize size)
{-# INLINE parseFixedSizeAndContinueWith #-}

capturedPacketLengthMA :: MArray.MutArray Word8 -> IO Int
capturedPacketLengthMA bs =
    fromIntegral <$> (fourOctetsToWord32 <$> MArray.getIndexUnsafe 11 bs <*> MArray.getIndexUnsafe 10 bs <*> MArray.getIndexUnsafe 9 bs <*> MArray.getIndexUnsafe 8 bs)
{-# INLINE capturedPacketLengthMA #-}
