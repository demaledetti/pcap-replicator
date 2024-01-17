module PcapReplicator.Cli (Options(..), parseCli) where

import HaskellWorks.Data.Network.Ip.Ipv4 (IpAddress(..))
import Network.Socket (PortNumber)
import Options.Applicative

import PcapReplicator


data Options = Options
  { ip                :: !IpAddress
  , port              :: !PortNumber
  , pcapParserName    :: !PcapParserName
  , bufferBytes       :: !Int
  , readBufferBytes   :: !Int
  , once              :: !Bool
  , cmd               :: ![String]
  } deriving Show

cli :: Parser Options
cli = Options
      -- server options
      <$> option auto
          ( long "ip"
         <> short 'i'
         <> help "IP address to listen to"
         <> showDefault
         <> value (IpAddress 0)
         <> metavar "IP" )
      <*> option auto
          ( long "port"
         <> short 'p'
         <> help "Port to listen to"
         <> showDefault
         <> value 8091
         <> metavar "PORT" )
      -- benchmarking options
      <*> option auto
          ( long "parser"
         <> short 'P'
         <> help "Pcap parser to use (for benchmarking)"
         <> showDefault
         <> value ByteString
         <> metavar "PARSER" )
      <*> option auto
          ( long "bufferBytes"
         <> short 'b'
         <> help "Size of send buffer in bytes (for benchmarking)"
         <> showDefault
         <> value (32 * 1024)
         <> metavar "BUFFER" )
      <*> option auto
          ( long "readBufferBytes"
         <> short 'r'
         <> help "Size of read buffer in bytes (for benchmarking)"
         <> showDefault
         <> value (32 * 1024)
         <> metavar "RBUFFER" )
      <*> switch
          ( long "once"
         <> short '1'
         <> help "Run the CMD only once (for benchmarking)" )
      -- required arguments
      <*> some (argument str (metavar "CMD..."))

parseCli :: IO Options
parseCli = execParser opts
  where
    opts = info (cli <**> helper) fullDesc
