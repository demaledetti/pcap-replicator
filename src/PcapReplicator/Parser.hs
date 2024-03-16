module PcapReplicator.Parser (getParser) where

import PcapReplicator

import qualified PcapReplicator.Parser.Fold
import qualified PcapReplicator.Parser.FoldCP
import qualified PcapReplicator.Parser.Parser
import qualified PcapReplicator.Parser.Streaming
import qualified PcapReplicator.Parser.Unfold


getParser :: PcapParserName -> PcapParser
getParser name = case name of
    Fold -> PcapReplicator.Parser.Fold.parseF
    FoldCP -> PcapReplicator.Parser.FoldCP.parseFCP
    Parser -> PcapReplicator.Parser.Parser.parseP
    ParserCP -> PcapReplicator.Parser.Parser.parseCP
    ParserMA -> PcapReplicator.Parser.Parser.parseMA
    ParserChunked -> PcapReplicator.Parser.Parser.parsePChunked
    ParserCPChunked -> PcapReplicator.Parser.Parser.parseCPChunked
    ParserMAChunked -> PcapReplicator.Parser.Parser.parseMAChunked
    ByteString -> PcapReplicator.Parser.Streaming.parseBS
    Array -> PcapReplicator.Parser.Streaming.parseA
    Unfold -> PcapReplicator.Parser.Unfold.parseU
