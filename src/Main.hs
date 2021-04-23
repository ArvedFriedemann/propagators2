module Main where

import "this" Control.Language.PipelineExec
import "this" Tests.TestsExec
import "this" Tests.CircuitExec
--import "this" Tests.SimpleTests
import "this" Violet.Basics
import "this" Violet.Parser

main :: IO ()
main = do
  putStrLn "Welcome to Propagators2 Main!"
  putStrLn "Please enter a file name for parsing"
  putStrLn "Please note that through a current bug in the system, the proof search might not terminate. Also due to the bug, we currently constrain ourselves to do only exactly one proof step (applying the first rule). We apologise for the inconvenience"
  putStrLn "The instance from the paper is in "
  putStrLn "Instances/Concatenation"
  putStrLn "file name:"
  filename <- getLine
  parseFileAndPerformProofSearch filename
