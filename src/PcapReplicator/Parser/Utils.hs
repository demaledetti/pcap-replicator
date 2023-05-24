module PcapReplicator.Parser.Utils (capturedPacketLength, fourOctetsToWord32, mytake) where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Internal.Data.Array.Foreign as Array
import System.IO (Handle)


fourOctetsToWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
fourOctetsToWord32 a b c d =
    ( fromIntegral a `shiftL` 24) .|.
    ( fromIntegral b `shiftL` 16) .|.
    ( fromIntegral c `shiftL`  8) .|.
      fromIntegral d
{-# INLINE fourOctetsToWord32 #-}

mytake :: Handle -> Int -> IO (Array.Array Word8)
mytake handle size = Strict.toArray <$> BS.hGet handle size
{-# INLINE mytake #-}

capturedPacketLength :: Array.Array Word8 -> Int
capturedPacketLength bs =
    fromIntegral $ fourOctetsToWord32 (Array.unsafeIndex 11 bs) (Array.unsafeIndex 10 bs) (Array.unsafeIndex 9 bs) (Array.unsafeIndex 8 bs)
{-# INLINE capturedPacketLength #-}
