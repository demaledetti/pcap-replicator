{-# LANGUAGE RecordWildCards #-}

module PcapReplicator.Cli (Options (..), parseCli, PerformanceTunables (..), ServerOptions (..), toArgs) where

import Data.Textual (toString)
import Network.IP.Addr (IP, IP46 (..), anyIP6)
import Network.Socket (PortNumber)
import Options.Applicative

import PcapReplicator

data Options = Options
    { server :: !ServerOptions
    , tunables :: !PerformanceTunables
    , once :: !Bool
    , cmd :: ![String]
    }
    deriving (Show)

data ServerOptions = ServerOptions
    { ip :: !IP
    , port :: !PortNumber
    }
    deriving (Show)

data PerformanceTunables = PerformanceTunables
    { pcapParserName :: !PcapParserName
    , bufferBytes :: !Int
    , readBufferBytes :: !Int
    }
    deriving (Show)

toArgs :: PerformanceTunables -> [String]
toArgs PerformanceTunables{..} =
    [ "-P"
    , show pcapParserName
    , "-b"
    , show bufferBytes
    , "-r"
    , show readBufferBytes
    ]

cli :: Int -> Parser Options
cli defaultBufferSize =
    Options
        -- server options
        <$> parserOptionGroup
            "Server options:"
            ( ServerOptions
                <$> option
                    auto
                    ( long "ip"
                        <> short 'i'
                        <> help "IP address to listen to"
                        <> showDefaultWith toString
                        <> value (IPv6 anyIP6)
                        <> metavar "IP"
                    )
                <*> option
                    auto
                    ( long "port"
                        <> short 'p'
                        <> help "Port to listen to"
                        <> showDefault
                        <> value 8091
                        <> metavar "PORT"
                    )
            )
        -- benchmarking options
        <*> parserOptionGroup
            "Performance tunables:"
            ( PerformanceTunables
                <$> option
                    auto
                    ( long "parser"
                        <> short 'P'
                        <> help "Pcap parser to use"
                        <> showDefault
                        <> value Unfold
                        <> metavar "PARSER"
                    )
                <*> option
                    auto
                    ( long "bufferBytes"
                        <> short 'b'
                        <> help "Size of send buffer in bytes"
                        <> showDefault
                        <> value defaultBufferSize
                        <> metavar "BUFFER"
                    )
                <*> option
                    auto
                    ( long "readBufferBytes"
                        <> short 'r'
                        <> help "Size of read buffer in bytes"
                        <> showDefault
                        <> value (32 * 1024)
                        <> metavar "RBUFFER"
                    )
            )
        <*> switch
            ( long "once"
                <> short '1'
                <> help "Run the CMD only once (for benchmarking)"
            )
        -- required arguments
        <*> some (argument str (metavar "CMD..."))

parseCli :: Int -> IO Options
parseCli defaultBufferSize = execParser opts
  where
    opts =
        info
            (cli defaultBufferSize <**> helper)
            ( fullDesc
                <> footer "The performance tunable defaults have been chosen by benchmarking"
            )
