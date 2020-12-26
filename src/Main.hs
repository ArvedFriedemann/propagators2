{-# LANGUAGE QuasiQuotes #-}
module Main where

--import "base" Debug.Trace

--import "containers" Data.Set ( Set )

--import "this" Control.Propagator
-- import "this" Data.Constraints ( con )
--import "this" Data.Num1 () -- Num (Set a)

import "this" Tests.UnificationTest


main :: IO ()
main = test1

{-
type IntFacts = Set Int


main :: IO ()
main = print . runSimplePropagator $ do
    a <- newCell @_ @IntFacts "a" [1, 2]
    watch a $ \ va -> traceM ("called a " ++ show va)
    b <- newCell @_ @IntFacts "b" [2, 3]
    watch b $ \ vb -> traceM ("called b " ++ show vb)
   
    traceM "eq a b"
    eq a b
    
    traverse @[] readCell [a, b]
-}