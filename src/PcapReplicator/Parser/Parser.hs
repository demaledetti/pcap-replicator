module PcapReplicator.Parser.Parser (parse, parse2) where

import Data.Word (Word8)
import qualified Streamly.FileSystem.Handle as H
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Parser as Parser
import Streamly.Internal.Data.Stream.IsStream (parseMany)
import qualified Streamly.Prelude as Stream

import PcapReplicator
import PcapReplicator.Parser.Utils


parse :: PcapParser
parse handle = parseMany parsePacket $ Stream.unfold H.read handle

parse2 :: PcapParser
parse2 handle = parseMany parsePacket2 $ Stream.unfold H.read handle

parsePacket :: Parser.Parser IO Word8 (Array.Array Word8)
parsePacket = do
    pcap_pkt_hdr <- parseFixedSize 16
    pcap_pkt_data <- parseFixedSize (capturedPacketLength pcap_pkt_hdr)
    pure $ pcap_pkt_hdr <> pcap_pkt_data

parsePacket2 :: Parser.Parser IO Word8 (Array.Array Word8)
parsePacket2 = parseFixedSizeAndContinueWith 16 $ \pcap_pkt_hdr ->
    Parser.serialWith (<>) (Parser.fromPure pcap_pkt_hdr)
                           (parseFixedSize (capturedPacketLength pcap_pkt_hdr))

parseFixedSize :: Int -> Parser.Parser IO Word8 (Array.Array Word8)
parseFixedSize size = Parser.takeEQ size (Array.writeN size)
{-# INLINE parseFixedSize #-}

parseFixedSizeAndContinueWith :: Int -> (Array.Array Word8 -> Parser.Parser IO Word8 (Array.Array Word8)) -> Parser.Parser IO Word8 (Array.Array Word8)
parseFixedSizeAndContinueWith size = flip Parser.concatMap (parseFixedSize size)
{-# INLINE parseFixedSizeAndContinueWith #-}
