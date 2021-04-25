module Main where

import "this" Control.Language.PipelineExec
import "this" Tests.TestsExec
import "this" Tests.CircuitExec
--import "this" Tests.SimpleTests
import "this" Violet.Basics
import "this" Violet.Parser
import "this" Violet.Compiler

import "base" Control.Concurrent

main :: IO ()
main = do
  putStrLn "Welcome to Propagators2 Main!"
  putStrLn "Please enter a file name for parsing"
  putStrLn "The instance from the paper is in "
  putStrLn "Instances/Concatenation"
  putStrLn "file name:"
  filename <- getLine
  parseFileAndPerformProofSearch filename
