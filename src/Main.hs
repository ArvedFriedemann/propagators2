module Main where

import "this" Tests.UnificationTest
import "this" Control.Language.Pipeline

main :: IO ()
main = do
  putStrLn "Hello and Welcome!"
  putStrLn "Please input a file to start the proof search."
  putStrLn "Note that the last term in your file will mark the goal for the proof search!"
  ln <- readLn
  putStrLn "Thanks! Let's get started!"
  parseFileAndPerformProofSearch (-1) ln

proofSearch :: String -> IO ()
proofSearch filename = parseFileAndPerformProofSearch (-1) filename

--
