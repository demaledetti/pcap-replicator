module PcapReplicator.Parser.Fold (parseF) where

import Data.Word (Word8)
import Streamly.Data.MutArray qualified as MArray
import Streamly.Data.StreamK qualified as StreamK
import Streamly.FileSystem.Handle qualified as H
import Streamly.Internal.Data.Array qualified as Array
import Streamly.Internal.Data.Array.Stream qualified as ArrayStream
import Streamly.Internal.Data.Fold qualified as Fold
import Streamly.Internal.Data.Fold.Chunked qualified as ChunkFold

import PcapReplicator
import PcapReplicator.Parser.Utils

data Next = NextIsHeader | NextIsPacket
data NextState = NextState !Next !Int !(MArray.MutArray Word8)

parseF :: PcapParser
parseF readBufferBytes handle =
    right
        <$> StreamK.toStream
            ( ArrayStream.runArrayFoldMany
                (ChunkFold.fromFold (Fold.Fold step inject extract extract))
                $ StreamK.fromStream
                $ H.readChunksWith readBufferBytes handle
            )
  where
    inject = Fold.Partial . NextState NextIsHeader 16 <$> MArray.new 4096
    extract (NextState _ _ s) = pure $! Array.unsafeFreeze s
    step (NextState NextIsHeader 1 acc) cur = do
        acc' <- MArray.snoc acc cur
        packetLength <- capturedPacketLengthMA acc'
        pure $! Fold.Partial (NextState NextIsPacket packetLength acc')
    step (NextState NextIsPacket 1 acc) cur = do
        acc' <- MArray.snoc acc cur
        pure $! Fold.Done $ Array.unsafeFreeze acc'
    step (NextState state remaining acc) cur = do
        acc' <- MArray.snoc acc cur
        pure $! Fold.Partial (NextState state (remaining - 1) acc')

capturedPacketLengthMA :: MArray.MutArray Word8 -> IO Int
capturedPacketLengthMA bs =
    fromIntegral
        <$> ( fourOctetsToWord32
                <$> MArray.getIndexUnsafe 11 bs
                <*> MArray.getIndexUnsafe 10 bs
                <*> MArray.getIndexUnsafe 9 bs
                <*> MArray.getIndexUnsafe 8 bs
            )
{-# INLINE capturedPacketLengthMA #-}
