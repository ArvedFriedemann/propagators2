module Control.Language.PipelineExec where

import "base" Debug.Trace

import "this" Control.Language.Pipeline
import "this" Control.Propagator.Implementation
import "this" Control.Propagator.References
import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions

import "transformers" Control.Monad.Trans.Class

import "stm" Control.Concurrent.STM hiding (atomically)
import qualified "stm" Control.Concurrent.STM as STM

parseFileAndPerformProofSearch :: String -> IO ()
parseFileAndPerformProofSearch filename = do
  s <- readFile filename
  runMonadPropIOFin (parseAndPerformProofSearch @IOSTMProp @(CellPtr STM Ref) @(Scope Ref) () s) (\goal -> do
    trm <- fromCellSize @IOSTMProp @(CellPtr STM Ref) @(Scope Ref) 100 goal
    ISP (lift $ putStrLn $ "Final Goal:\n" ++ (show trm))
    )
  return ()
