module Control.Language.PipelineExec where

import "this" Control.Language.Pipeline
import "this" Control.Propagator.Implementation
import "this" Control.Propagator.References

import "stm" Control.Concurrent.STM hiding (atomically)
import qualified "stm" Control.Concurrent.STM as STM

parseFileAndPerformProofSearch :: String -> IO ()
parseFileAndPerformProofSearch filename = do
  s <- readFile filename
  runMonadPropIO $ parseAndPerformProofSearch @IOSTMProp @(CellPtr STM Ref) @(Scope Ref) () s
  return ()
