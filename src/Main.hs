module Main where

import "this" Control.Language.PipelineExec
import "this" Tests.TestsExec
--import "this" Tests.SimpleTests

import "base" Control.Concurrent

import "formatting" Formatting
import "formatting" Formatting.Clock
import "clock" System.Clock

main :: IO ()
main = do
  setNumCapabilities 4
  putStrLn "Welcome to Propagators2 Main!"
  putStrLn "Please enter a file name for parsing"
  putStrLn "The instance from the paper is in "
  putStrLn "Instances/Concatenation"
  putStrLn "file name:"
  filename <- getLine
  start <- getTime Monotonic
  parseFileAndPerformProofSearch filename
  end <- getTime Monotonic
  fprint (timeSpecs % "\n") start end
