module PcapReplicator.Parser (getParser) where

import PcapReplicator

import qualified PcapReplicator.Parser.Fold
import qualified PcapReplicator.Parser.FoldCP
import qualified PcapReplicator.Parser.Parser
import qualified PcapReplicator.Parser.Streaming
import qualified PcapReplicator.Parser.Unfold


getParser :: PcapParserName -> PcapParser
getParser Fold = PcapReplicator.Parser.Fold.parse
getParser FoldCP = PcapReplicator.Parser.FoldCP.parse
getParser Parser = PcapReplicator.Parser.Parser.parse
getParser ParserCP = PcapReplicator.Parser.Parser.parse2
getParser ByteString = PcapReplicator.Parser.Streaming.parse
getParser Array = PcapReplicator.Parser.Streaming.parse2
getParser Unfold = PcapReplicator.Parser.Unfold.parse
