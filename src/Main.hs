{-# LANGUAGE QuasiQuotes #-}
module Main where

import "containers" Data.Set ( Set )

import "this" Control.Propagator
import "this" Data.Constraints ( con )
import "this" Data.Num1 () -- Num (Set a)


type IntFacts = Set Int

main :: IO ()
main = print . runSimplePropagator $ do
    a <- newCell @_ @IntFacts "a" [0 .. 24]
    b <- newCell @_ @IntFacts "b" [0 .. 24]
    c <- newCell @_ @IntFacts "c" [0 .. 24]
    [con| c = a + b |]
    write a 2
    write b 3
    traverse @[] readCell [a, b, c]
