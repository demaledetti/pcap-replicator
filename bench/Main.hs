{-# LANGUAGE RecordWildCards #-}

module Main where

-- many good ideas taken from:
-- https://gitlab.com/FinnBender/haskell-parsing-benchmarks

import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Streamly.Data.Array as A
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream
import System.FilePath ((</>))
import System.IO (IOMode (..), withBinaryFile)
import System.Process (callProcess)
import Test.Tasty (
    askOption,
    defaultMainWithIngredients,
    includingOptions,
    localOption,
    testGroup,
 )
import Test.Tasty.Bench
import Test.Tasty.Options
import Test.Tasty.Patterns.Printer (printAwkExpr)

import PcapReplicator
import PcapReplicator.Parser (getParser)
import PcapReplicator.Process (PcapProcess (..), pcapProcess)
import System.Environment (getExecutablePath, lookupEnv)

type BenchBuilder = PcapParser -> IO ()
data ProgBenchArgs = ProgBenchArgs PcapParserName -- Int

parsersUnderTest, parsersUnderTestProg :: [PcapParserName]
parsersUnderTest =
    [ StreamingAttoparsec
    , Binary
    -- , Unfold
    -- , Fold
    -- -- , FoldCP
    -- , ParserChunked
    -- , ParserMAChunked
    -- -- , ParserCPChunked
    -- -- , ParserMA
    -- -- , Parser
    -- -- , ParserCP
    ]
parsersUnderTestProg = parsersUnderTest

bestParserUnderTest :: String
bestParserUnderTest = "StreamingAttoparsec"

progsUnderTest :: [String]
progsUnderTest =
    [ "ioref"
    -- , "statet"
    ]

compareWithBaseline :: String -> String -> String -> Benchmark -> Benchmark
compareWithBaseline group name baseline benchmark =
    if name == baseline
        then benchmark
        else bcompare baselineBenchPat benchmark
  where
    baselineBenchPat = printAwkExpr $ locateBenchmark [baseline, group]

aMillion :: Int
aMillion = 10 ^ (6 :: Int)

newtype PacketMillions = PacketMillions Int

instance IsOption PacketMillions where
    defaultValue = PacketMillions 10
    parseValue = fmap PacketMillions . safeRead
    optionName = pure "packet-millions"
    optionHelp = pure "Millions of packets used in benchmarks"

main :: IO ()
main = do
    benchmarkBinaryPath <- getExecutablePath
    rtDirPath <- fromMaybe "." <$> lookupEnv "XDG_RUNTIME_DIR"
    let customOpts = [Option (Proxy :: Proxy PacketMillions)]
        ingredients = includingOptions customOpts : benchIngredients
        benchmarks =
            testGroup
                "All"
                [ fileParse rtDirPath
                , streamParse rtDirPath
                , progParse benchmarkBinaryPath
                , progSend benchmarkBinaryPath
                ]
    defaultMainWithIngredients ingredients benchmarks

makeParseBench
    :: String -> (FilePath -> Int -> BenchBuilder) -> FilePath -> Benchmark
makeParseBench name action rtDirPath = askOption $ \(PacketMillions pm) ->
    let
        pcapFilePath = rtDirPath </> "test" <> show pm <> ".pcap"
     in
        makeBenchGroup name bestParserUnderTest $ action pcapFilePath pm

makeBenchGroup :: String -> String -> BenchBuilder -> Benchmark
makeBenchGroup group baseline builder =
    bgroup group $ single <$> parsersUnderTest
  where
    single parserName =
        let parse = getParser parserName
            name = show parserName
            benchmark = bench name $ whnfIO $ builder parse
         in compareWithBaseline group name baseline benchmark

fileParse, streamParse :: FilePath -> Benchmark
fileParse = makeParseBench "FileParse" parseFile
streamParse = makeParseBench "StreamParse" parseStream

parseFile :: FilePath -> Int -> PcapParser -> IO ()
parseFile path packetMillions parse = do
    withBinaryFile path ReadMode $ \handle -> do
        void $ BS.hGet handle 24
        result <- Stream.fold Fold.length $ parse readChunkLength handle
        when (result /= packetMillions * aMillion) $
            error ("Wrong result: " ++ show result)
  where
    readChunkLength = 32 * 1024

parseStream :: FilePath -> Int -> PcapParser -> IO ()
parseStream path packetMillions parse = do
    PcapProcess{..} <-
        pcapProcess ("cat " ++ path) parse chunkLength readChunkLength
    mFirst <- Stream.uncons pcapPacketStream
    case mFirst of
        Just (chunk, rest) -> do
            let chunkLen = A.length chunk
            when (chunkLen `rem` packetLength /= 0) $
                error $
                    "Chunk size is not a multiple of packet size: " ++ show chunkLen
            result <- Stream.fold Fold.length rest
            when
                ( sourceLength `quot` chunkLen
                    + (if sourceLength `rem` chunkLen == 0 then 0 else 1)
                    /= result + 1
                )
                $ error
                $ "Wrong number of chunks: " ++ show (result + 1)
        Nothing -> error "Empty stream"
  where
    chunkLength = 64 * 1024
    readChunkLength = 32 * 1024
    packetLength = 66
    packets = packetMillions * aMillion
    sourceLength = packetLength * packets

makeProgBenchGroup :: String -> String -> String -> String -> Benchmark
makeProgBenchGroup group baseline app bbpath =
    localOption WallTime $ askOption $ \(PacketMillions pm) ->
        let
            makeProgBench :: String -> [String] -> Benchmark
            makeProgBench name appArgs =
                let nameWithArgs = name ++ if null appArgs then "" else " " ++ unwords appArgs
                    benchmark =
                        bench nameWithArgs $
                            whnfIO $
                                callProcess "./bench.sh" $
                                    [name, "-s", show pm, "-q", "-b", bbpath, "--"] ++ appArgs
                 in compareWithBaseline group name baseline benchmark

            multiProgBench :: String -> [Benchmark]
            multiProgBench name = makeProgBench name . toArgs <$> combinations
              where
                combinations =
                    ProgBenchArgs
                        <$> parsersUnderTestProg
                -- <*> ((* 1024) <$> [64]) -- [{-0, 16,32,-} 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16*1024])
                toArgs (ProgBenchArgs parserName) =
                    ["-P", show parserName]
         in
            -- toArgs (ProgBenchArgs parserName bufferSize) =
            --     ["-P", show parserName, "-b", show bufferSize]

            bgroup
                group
                ( makeProgBench baseline []
                    : concatMap (\impl -> multiProgBench (app <> "-" <> impl)) progsUnderTest
                )

progParse, progSend :: String -> Benchmark
progParse = makeProgBenchGroup "ProgParse" "tcpdump" "app"
progSend = makeProgBenchGroup "ProgSend" "tcpdump_tcpdump" "app_tcpdump"
