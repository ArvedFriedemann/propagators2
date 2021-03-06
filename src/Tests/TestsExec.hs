{-# LANGUAGE NoImplicitPrelude #-}
module Tests.TestsExec where

import "base" Prelude hiding ( read )
import "base" Data.Typeable
import "base" Debug.Trace
import "base" Control.Monad

import "this" Control.Propagator.Class
import "this" Control.Propagator.Implementation
import "this" Control.Propagator.References
import "this" Data.Lattice
import "this" Data.Some
import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions

import "this" Tests.SimpleTests

import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set

import "stm" Control.Concurrent.STM hiding (atomically)
import qualified "stm" Control.Concurrent.STM as STM

import "transformers" Control.Monad.Trans.Class

--(MonadProp IOSTMProp (CellPtr STM Ref) (Scope Ref)) =>
test' :: IO String
test' = runMonadPropIO @String (test @IOSTMProp @(CellPtr STM Ref) @(Scope Ref))

stdTest :: IOSTMProp [TermSetPtr (CellPtr STM Ref)] -> IO ()
stdTest act = void $ runMonadPropIOFin act $ \ptrs -> do
  forM_ ptrs $ \p -> do
    trm <- fromCellSize 100 p
    ISP $ lift $ putStrLn $ show trm

test2' :: IO ()
test2' = stdTest test2

test3' :: IO ()
test3' = stdTest test3

test4' :: IO ()
test4' = stdTest test4

test5' :: IO ()
test5' = stdTest test5

test6' :: IO ()
test6' = stdTest test6

test7' :: IO ()
test7' = stdTest test7

test8' :: IO ()
test8' = stdTest test8

test9' :: IO ()
test9' = stdTest test9






--
