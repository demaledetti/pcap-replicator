module PcapReplicator.Parser.Fold (parse) where

import Data.Word (Word8)
import qualified Streamly.FileSystem.Handle as H
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MArray
import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream
import qualified Streamly.Internal.Data.Array.Stream.Fold.Foreign as ArrayFold
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Prelude as Stream

import PcapReplicator
import PcapReplicator.Parser.Utils


data Next = NextIsHeader | NextIsPacket
data NextState = NextState !Next !Int !(MArray.Array Word8)

parse :: PcapParser
parse handle =
     ArrayStream.foldArrMany (ArrayFold.fromFold (Fold.mkFoldM step inject extract))
       $ Stream.unfold H.readChunks handle
  where
    inject = Fold.Partial . NextState NextIsHeader 16 <$> MArray.newArray 4096
    extract (NextState _ _ s) = pure (Array.unsafeFreeze s)
    step (NextState NextIsHeader 1 acc) cur = do
        acc' <- MArray.snocUnsafe acc cur
        packetLength <- capturedPacketLengthMA acc'
        pure $ Fold.Partial (NextState NextIsPacket packetLength acc')
    step (NextState NextIsPacket 1 acc) cur = do
        acc' <- MArray.snocUnsafe acc cur
        pure $ Fold.Done $ Array.unsafeFreeze acc'
    step (NextState state remaining acc) cur = do
        acc' <- MArray.snocUnsafe acc cur
        pure $ Fold.Partial (NextState state (remaining - 1) acc')

capturedPacketLengthMA :: MArray.Array Word8 -> IO Int
capturedPacketLengthMA bs =
    fromIntegral <$> (fourOctetsToWord32 <$> MArray.getIndexUnsafe 11 bs <*> MArray.getIndexUnsafe 10 bs <*> MArray.getIndexUnsafe 9 bs <*> MArray.getIndexUnsafe 8 bs)
{-# INLINE capturedPacketLengthMA #-}
