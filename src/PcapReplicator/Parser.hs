module PcapReplicator.Parser (offline, Packet(..)) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Streaming as AS
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Word (Word32)
import GHC.IO.Handle (Handle)
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as Streaming


-- The following bits are humbly adapted from
-- <https://hackage.haskell.org/package/streaming-pcap streaming-pcap> by Colin Woodbury

data Packet = Packet { header :: BS.ByteString, bytes :: BS.ByteString }

packetP :: A.Parser Packet
packetP = do
  hdr <- A.take 16
  bts <- A.take . fromIntegral . four . BS.take 4 $ BS.drop 8 hdr
  pure $ Packet hdr bts

four :: BS.ByteString -> Word32
four = Get.runGet Get.getWord32le . LB.fromStrict
{-# INLINE four #-}

offline :: MonadIO m => Handle -> Streaming.Stream (Streaming.Of Packet) m ()
offline = void . AS.parsed packetP . Q.fromHandle
