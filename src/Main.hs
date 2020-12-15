{-# LANGUAGE QuasiQuotes #-}
module Main where

import "this" Control.Propagator
import "this" Data.Facts
import "this" Data.Constraints ( con )


type NumFacts m n = Facts (OrdFact (Cell m n) n)

main :: IO ()
main = print . runMyProp $ do
    a <- newEmptyCell @(NumFacts MyProp Int)
    b <- newEmptyCell @(NumFacts MyProp Int)
    c <- newEmptyCell @(NumFacts MyProp Int)
    [con| c = a + b |]
    write a 2
    write b 3
    traverse @[] readCell [a, b, c]
