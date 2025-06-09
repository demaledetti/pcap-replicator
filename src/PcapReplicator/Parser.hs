module PcapReplicator.Parser (getParser) where

import PcapReplicator

import PcapReplicator.Parser.Binary qualified
import PcapReplicator.Parser.Fold qualified
import PcapReplicator.Parser.FoldCP qualified
import PcapReplicator.Parser.Parser qualified
import PcapReplicator.Parser.Streaming qualified
import PcapReplicator.Parser.Unfold qualified

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
    StreamingAttoparsec -> PcapReplicator.Parser.Streaming.parse
    Binary -> PcapReplicator.Parser.Binary.parse
    Unfold -> PcapReplicator.Parser.Unfold.parseU
    UnfoldChunked -> PcapReplicator.Parser.Unfold.parse
