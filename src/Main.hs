module Main where

import "this" Tests.UnificationTest


main :: IO ()
main = do
    printTest 1 >> test1
    printTest 2 >> test2
    printTest 3 >> test3
  where
    printTest :: Int -> IO ()
    printTest i = do
        putStrLn (replicate 80 '-')
        putStrLn $ "-- test" ++ show i
        putStrLn (replicate 80 '-')
