{-# LANGUAGE OverloadedRecordDot #-}

module PcapReplicator.Main (main) where

import PcapReplicator
import PcapReplicator.Cli
import PcapReplicator.Log
import PcapReplicator.State.IORef qualified as IORef
import PcapReplicator.State.StateT qualified as StateT

main :: IO ()
main = do
    options <- parseCli
    print options
    let stateImplMain =
            case options.tunables.stateImplementation of
                IORef -> IORef.main
                StateT -> StateT.main
     in stateImplMain options stdoutIOTextTracer
