module PcapReplicator.Parser (
    PcapParserImplementation (..),
    PcapParserName (..),
    getParser,
    mkPcapParserImplementation,
) where

import PcapReplicator

import PcapReplicator.Parser.Binary qualified
import PcapReplicator.Parser.None qualified
import PcapReplicator.Parser.Streaming qualified
import PcapReplicator.Parser.Unfold qualified

data PcapParserName
    = Unfold
    | StreamingAttoparsec
    | Binary
    | None
    deriving (Read, Show)

data PcapParserImplementation = PcapParserImplementation
    { name :: !PcapParserName
    , parser :: !PcapParser
    }

instance Show PcapParserImplementation where
    show = show . name

getParser :: PcapParserName -> PcapParser
getParser name = case name of
    None -> PcapReplicator.Parser.None.parse
    StreamingAttoparsec -> PcapReplicator.Parser.Streaming.parse
    Binary -> PcapReplicator.Parser.Binary.parse
    Unfold -> PcapReplicator.Parser.Unfold.parse

mkPcapParserImplementation :: PcapParserName -> PcapParserImplementation
mkPcapParserImplementation name = PcapParserImplementation name $ getParser name
