{-# LANGUAGE OverloadedRecordDot #-}
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
import PcapReplicator.Cli (PerformanceTunables (..), toArgs)
import PcapReplicator.Parser
import PcapReplicator.Process (PcapProcess (..), pcapProcess)
import System.Environment (getExecutablePath, lookupEnv)

type BenchBuilder = BenchEnv -> PerformanceTunables -> IO ()

parsersUnderTest, parsersUnderTestProg :: [PcapParserImplementation]
parsersUnderTest =
    mkPcapParserImplementation
        <$> [ Unfold
            , StreamingAttoparsec
            -- , Binary
            ]
-- XXX None works only for Prog benchmarks
parsersUnderTestProg = parsersUnderTest -- <> [mkPcapParserImplementation None]

stateImplsUnderTest :: [StateImplementationName]
stateImplsUnderTest =
    [ IORef
    -- , StateT
    ]

combinations, combinationsProg :: [PerformanceTunables]
combinations =
    PerformanceTunables
        <$> stateImplsUnderTest
        <*> parsersUnderTest
        <*> [k 64]
        <*> [k 32]
combinationsProg =
    PerformanceTunables
        <$> stateImplsUnderTest
        <*> parsersUnderTestProg
        <*> [k 64]
        <*> [k 32]

bestParserUnderTest :: String
bestParserUnderTest = "Unfold"

bestBufferSize :: StateImplementationName -> Int
bestBufferSize IORef = k 64
bestBufferSize StateT = k 256

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

data BenchEnv = BenchEnv
    { packetMillions :: !Int
    , pcapFilePath :: !FilePath
    , benchmarkBinaryPath :: !FilePath
    }
main :: IO ()
main = do
    benchmarkBinaryPath <- getExecutablePath
    rtDirPath <- fromMaybe "." <$> lookupEnv "XDG_RUNTIME_DIR"
    let customOpts = [Option (Proxy :: Proxy PacketMillions)]
        ingredients = includingOptions customOpts : benchIngredients
        benchmarks = askOption $ \(PacketMillions packetMillions) ->
            let
                pcapFilePath = rtDirPath </> "test" <> show packetMillions <> ".pcap"
                benchEnv = BenchEnv{..}
             in
                testGroup
                    "All"
                    ( [ fileParse
                      , streamParse
                      , progParse
                      , progSend
                      ]
                        <*> [benchEnv]
                    )
    defaultMainWithIngredients ingredients benchmarks

fileParse, streamParse, progParse, progSend :: BenchEnv -> Benchmark
fileParse = makeBenchGroup "FileParse" parseFile
streamParse = makeBenchGroup "StreamParse" parseStream
progParse = makeProgBenchGroup "ProgParse" "tcpdump" "app"
progSend = makeProgBenchGroup "ProgSend" "tcpdump_tcpdump" "app_tcpdump"

makeBenchGroup :: String -> BenchBuilder -> BenchEnv -> Benchmark
makeBenchGroup group builder benv =
    bgroup group $ single <$> combinations
  where
    single pt@PerformanceTunables{..} =
        let name = show pcapParserImplementation.name
            benchmark = bench name $ whnfIO $ builder benv pt
         in compareWithBaseline group name bestParserUnderTest benchmark

makeProgBenchGroup :: String -> String -> String -> BenchEnv -> Benchmark
makeProgBenchGroup group baseline app BenchEnv{..} =
    localOption WallTime $
        let
            makeProgBench :: String -> [String] -> Benchmark
            makeProgBench name appArgs =
                let nameWithArgs = unwords $ name : appArgs
                    benchmark =
                        bench nameWithArgs $
                            whnfIO $
                                callProcess "./bench.sh" $
                                    [name, "-s", show packetMillions, "-q", "-b", benchmarkBinaryPath, "--"]
                                        ++ appArgs
                 in compareWithBaseline group name baseline benchmark

            multiProgBench :: String -> [Benchmark]
            multiProgBench name = makeProgBench name . toArgs <$> combinationsProg
         in
            bgroup
                group
                ( makeProgBench baseline []
                    : multiProgBench app
                )

parseFile :: BenchBuilder
parseFile BenchEnv{..} PerformanceTunables{..} = do
    withBinaryFile pcapFilePath ReadMode $ \handle -> do
        void $ BS.hGet handle 24
        result <-
            Stream.fold Fold.length $ pcapParserImplementation.parser readBufferBytes handle
        when (result /= packetMillions * aMillion) $
            error ("Wrong result: " ++ show result)

parseStream :: BenchBuilder
parseStream BenchEnv{..} PerformanceTunables{..} = do
    PcapProcess{..} <-
        pcapProcess
            ("cat " ++ pcapFilePath)
            pcapParserImplementation.parser
            bufferBytes
            readBufferBytes
    mFirst <- Stream.uncons pcapPacketStream
    case mFirst of
        Just (chunk, rest) -> do
            let chunkLength = A.length chunk
            when (chunkLength `rem` packetLength /= 0) $
                error $
                    "Chunk size is not a multiple of packet size: " ++ show chunkLength
            result <- Stream.fold Fold.length rest
            when
                ( sourceLength `quot` chunkLength
                    + (if sourceLength `rem` chunkLength == 0 then 0 else 1)
                    /= result + 1
                )
                $ error
                $ "Wrong number of chunks: " ++ show (result + 1)
        Nothing -> error "Empty stream"
  where
    packetLength = 66
    packets = packetMillions * aMillion
    sourceLength = packetLength * packets

k :: Int -> Int
k = (* 1024)
