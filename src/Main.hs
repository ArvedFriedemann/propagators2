module Main where

import "this" Control.Language.PipelineExec
import "this" Tests.TestsExec
--import "this" Tests.SimpleTests

import "base" Control.Concurrent
import "base" Control.Monad
import "timeit" System.TimeIt
import "dsp" Numeric.Statistics.Moment

main :: IO ()
main = do
  putStrLn "Welcome to Propagators2 Main!"
  putStrLn "Please enter a file name for parsing"
  putStrLn "The instance from the paper is in "
  putStrLn "Instances/Concatenation"
  putStrLn "file name:"
  filename <- getLine
  times <- forM [1..10] $ const $ fst <$> timeItT (parseFileAndPerformProofSearch filename)
  --parseFileAndPerformProofSearch filename
  putStrLn $ "Mean time: "++(show $ mean times)++"s"
  getLine
  main
