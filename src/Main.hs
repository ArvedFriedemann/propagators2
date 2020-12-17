{-# LANGUAGE QuasiQuotes #-}
module Main where

import "this" Control.Propagator
import "this" Data.Facts
import "this" Data.Constraints ( con )


type IntFacts = Facts (OrdFact (Cell SimplePropagator Int) Int)

main :: IO ()
main = print . runSimplePropagator $ do
    a <- newEmptyCell @IntFacts
    b <- newEmptyCell @IntFacts
    c <- newEmptyCell @IntFacts
    [con| c = a + b |]
    write a 2
    write b 3
    traverse @[] readCell [a, b, c]
