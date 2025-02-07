module PcapReplicator.Parser.Utils (capturedPacketLength, capturedPacketLengthBS, fourOctetsToWord32, mytake) where

import Data.Bits (unsafeShiftL, (.|.))
import Data.ByteString qualified as BS
import Data.Word (Word32, Word8)
import Streamly.External.ByteString qualified as Strict
import Streamly.Internal.Data.Array qualified as Array
import System.IO (Handle)

fourOctetsToWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
fourOctetsToWord32 a b c d =
    (fromIntegral a `unsafeShiftL` 24)
        .|. (fromIntegral b `unsafeShiftL` 16)
        .|. (fromIntegral c `unsafeShiftL` 8)
        .|. fromIntegral d
{-# INLINE fourOctetsToWord32 #-}

mytake :: Handle -> Int -> IO (Array.Array Word8)
mytake handle size = Strict.toArray <$> BS.hGet handle size
{-# INLINE mytake #-}

capturedPacketLength :: Array.Array Word8 -> Int
capturedPacketLength bs =
    fromIntegral $!
        fourOctetsToWord32
            (Array.getIndexUnsafe 11 bs)
            (Array.getIndexUnsafe 10 bs)
            (Array.getIndexUnsafe 9 bs)
            (Array.getIndexUnsafe 8 bs)
{-# INLINE capturedPacketLength #-}

capturedPacketLengthBS :: BS.ByteString -> Int
capturedPacketLengthBS bs =
    fromIntegral $!
        fourOctetsToWord32
            (BS.index bs 11)
            (BS.index bs 10)
            (BS.index bs 9)
            (BS.index bs 8)
{-# INLINE capturedPacketLengthBS #-}
