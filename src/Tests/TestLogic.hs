module Tests.TestLogic where

import "base" Data.List


import "this" Data.Terms.TermFunctions
import "this" Control.Propagator
import "this" Control.Propagator.Conc
import "this" Control.Propagator.Event

runTestSEB :: SEB [TermCell SEB] -> IO ()
runTestSEB p = (putStrLn =<<) (runSEB p showAll)

runTest :: Par [TermCell Par] -> IO ()
runTest p = (putStrLn =<<) (execPar p showAll)

showAll :: (Monad m, PropagatorMonad m) => [TermCell m] -> m String
showAll = fmap (intercalate "\n\n") . traverse (fmap show . fromCellSize @String 100)
