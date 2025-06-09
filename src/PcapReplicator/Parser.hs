module PcapReplicator.Parser (getParser) where

import PcapReplicator

import PcapReplicator.Parser.Binary qualified
import PcapReplicator.Parser.None qualified
import PcapReplicator.Parser.Streaming qualified
import PcapReplicator.Parser.Unfold qualified

getParser :: PcapParserName -> PcapParser
getParser name = case name of
    None -> PcapReplicator.Parser.None.parse
    StreamingAttoparsec -> PcapReplicator.Parser.Streaming.parse
    Binary -> PcapReplicator.Parser.Binary.parse
    Unfold -> PcapReplicator.Parser.Unfold.parse
