module PcapReplicator.Parser.None (parse) where

import Streamly.FileSystem.Handle qualified as H

import PcapReplicator

parse :: PcapParser
parse = H.readChunksWith . getReadBufferBytes
