module Main where

import "this" Control.Propagator
import "this" Data.Domain
-- import "this" Data.Facts


main :: IO ()
main = print . runMyProp $ do
    a <- newCell ([0 .. 12] :: Domain Int)
    b <- newCell ([0 .. 12] :: Domain Int)
    c <- newCell ([0 .. 12] :: Domain Int)
    link2 a b c (+)
    link2 c a b (-)
    link2 b c a (-)
    write a [0 .. 2]
    write b [0 .. 2]
    traverse @[] readCell [a, b, c]
