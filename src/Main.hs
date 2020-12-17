{-# LANGUAGE QuasiQuotes #-}
module Main where

import "containers" Data.Set ( Set )

import "this" Control.Propagator
import "this" Data.Facts
import "this" Data.Constraints ( con )


type IntFacts = Set Int

main :: IO ()
main = print . runSimplePropagator $ do
    a <- newCell @IntFacts [0 .. 24]
    b <- newCell @IntFacts [0 .. 24]
    c <- newCell @IntFacts [0 .. 24]
    [con| c = a + b |]
    write a 2
    write b 3
    traverse @[] readCell [a, b, c]
