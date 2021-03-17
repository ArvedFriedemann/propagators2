{-# LANGUAGE NoImplicitPrelude #-}
module Tests.TestsExec where

import "base" Prelude hiding ( read )
import "base" Data.Typeable
import "base" Debug.Trace

import "this" Control.Propagator.Class
import "this" Control.Propagator.Implementation
import "this" Control.Propagator.References
import "this" Data.Lattice
import "this" Data.Some

import "this" Tests.SimpleTests

import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set

import "stm" Control.Concurrent.STM hiding (atomically)
import qualified "stm" Control.Concurrent.STM as STM


--(MonadProp IOSTMProp (CellPtr STM Ref) (Scope Ref)) =>
test' :: IO String
test' = runMonadPropIO @String (test @IOSTMProp @(CellPtr STM Ref) @(Scope Ref))
--test' :: IO String
--test' = runMonadPropIO test2

{-}
test2 :: IOSTMProp String
test2 = do
  a <- (new (FSP @String $ Some ("a" :: String))) :: IOSTMProp (CellPtr STM Ref (FactSet String))
  b <- new (FSP @String $ Some ("b" :: String))
  write a (FS $ Set.singleton ("Test" :: String))
  watch a (MonadPointer @IOSTMProp (Some ("dirEq" :: String))) (read a >>= write b)
  watch a (MonadPointer @IOSTMProp (Some ("trace" :: String)))
    (read a >>= \a'-> traceM $ "A is " ++ show a')
  return "succeeded"
-}

--
