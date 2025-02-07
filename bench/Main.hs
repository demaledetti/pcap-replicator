{-# LANGUAGE RecordWildCards #-}
module Main where

-- many good ideas taken from:
-- https://gitlab.com/FinnBender/haskell-parsing-benchmarks

import Control.Monad (void, when)
import qualified Data.ByteString as BS
import Test.Tasty.Bench
import Test.Tasty (localOption)
import Test.Tasty.Patterns.Printer (printAwkExpr)
import Streamly.Data.Array as A
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import System.IO (IOMode(..), withBinaryFile)
import System.Process (callProcess)

import PcapReplicator
import PcapReplicator.Parser (getParser)
import PcapReplicator.Process (PcapProcess(..), pcapProcess)
import System.Environment (getExecutablePath)

parsersUnderTest, parsersUnderTestProg :: [PcapParserName]
parsersUnderTest = [ ByteString
                   , Array
                   -- , Unfold
                   -- , Fold
                   -- , FoldCP
                   -- , ParserMAChunked
                   -- , ParserChunked
                   -- , ParserCPChunked
                   -- , ParserMA
                   -- , Parser
                   -- , ParserCP
                   ]
parsersUnderTestProg = parsersUnderTest

bestParserUnderTest :: String
bestParserUnderTest = "ByteString"

testFile :: FilePath
testFile = "/home/ste/opt/haskell/rws/the10.pcap"

main :: IO ()
main = do
    benchmarkBinaryPath <- getExecutablePath
    defaultMain [fileParse, streamParse, progParse benchmarkBinaryPath, progSend benchmarkBinaryPath]

fileParse, streamParse :: Benchmark
fileParse = makeBenchGroup "FileParse" bestParserUnderTest $ parseFile testFile
streamParse = makeBenchGroup "StreamParse" bestParserUnderTest $ parseStream testFile

progParse :: String -> Benchmark
progParse bbpath = localOption WallTime $ bgroup
                 "ProgParse"
                 (makeProgBench bbpath "tcpdump" "tcpdump" []
                 : multiProgBench bbpath "app-ioref" "tcpdump"
                 ++ multiProgBench bbpath "app-statet" "tcpdump"
                 )

progSend :: String -> Benchmark
progSend bbpath = localOption WallTime $ bgroup
                 "ProgSend"
                 (makeProgBench bbpath "tcpdump_tcpdump" "tcpdump_tcpdump" []
                 : multiProgBench bbpath "app_tcpdump-ioref" "tcpdump_tcpdump"
                 ++ multiProgBench bbpath "app_tcpdump-statet" "tcpdump_tcpdump"
                 )

parseFile :: FilePath -> PcapParser -> IO ()
parseFile path parse = do
    withBinaryFile path ReadMode $ \handle -> do
      void $ BS.hGet handle 24
      result <- Stream.fold Fold.length $ parse readChunkLength handle
      when (result /= 10000000) $ error ("Wrong result: " ++ show result)
  where
    readChunkLength = 32 * 1024

parseStream :: FilePath -> PcapParser -> IO ()
parseStream path parse = do
    PcapProcess{..} <- pcapProcess ("cat " ++ path) parse chunkLength readChunkLength
    mFirst <- Stream.uncons pcapPacketStream
    case mFirst of
        Just (chunk, rest) -> do
            let chunkLen = A.length chunk
            when (chunkLen `rem` packetLength /= 0)
              $ error $ "Chunk size is not a multiple of packet size: " ++ show chunkLen
            result <- Stream.fold Fold.length rest
            when (sourceLength `quot` chunkLen + (if sourceLength `rem` chunkLen == 0 then 0 else 1) /= result + 1)
              $ error $ "Wrong number of chunks: " ++ show (result + 1)
        Nothing -> error "Empty stream"
  where
    chunkLength = 64 * 1024
    readChunkLength = 4096 * 1024
    packetLength = 66
    packets = 10000000
    sourceLength = packetLength * packets

type BenchBuilder = PcapParser -> IO ()

makeBenchGroup :: String -> String -> BenchBuilder -> Benchmark
makeBenchGroup group baseline builder =
    bgroup group $ single <$> parsersUnderTestProg
  where
    single parserName =
        let baselineExpr = group ++ "." ++ baseline
            parse = getParser parserName
            name = show parserName
            benchmark = bench name $ whnfIO $ builder parse
         in if name == baseline then benchmark else bcompare baselineExpr benchmark

makeProgBench :: String -> String -> String -> [String] -> Benchmark
makeProgBench bbpath name baseline appArgs =
    let nameWithArgs = name ++ if null appArgs then "" else " " ++ unwords appArgs
        benchmark = bench nameWithArgs $ whnfIO $ callProcess "./bench.sh" $ [name, "-s", "10", "-q", "-b", bbpath, "--"] ++ appArgs
     in compareWithBaseline name baseline benchmark

multiProgBench :: String -> String -> String -> [Benchmark]
multiProgBench bbpath name baseline = makeProgBench bbpath name baseline . toArgs <$> combinations
  where
    combinations = ProgBenchArgs
                    <$> parsersUnderTest
                    <*> ((*1024) <$> [64]) --[{-0, 16,-} 32, 64, 128, 256])
    toArgs (ProgBenchArgs parserName bufferSize) =
          ["-P", show parserName, "-b", show bufferSize]

data ProgBenchArgs = ProgBenchArgs PcapParserName Int

compareWithBaseline :: String -> String -> Benchmark -> Benchmark
compareWithBaseline name baseline benchmark =
    if name == baseline
            then benchmark
            else bcompare (baselineBenchPat baseline) benchmark

baselineBenchPat :: String -> String
baselineBenchPat name = printAwkExpr $ locateBenchmark [name]
