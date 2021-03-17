{-# LANGUAGE NoImplicitPrelude #-}
module Tests.TestsExec where

import "base" Prelude hiding ( read )
import "base" Data.Typeable
import "base" Debug.Trace

import "this" Control.Propagator.Class
import "this" Control.Propagator.Implementation
import "this" Data.Lattice
import "this" Data.Some

import "this" Tests.SimpleTests

import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set



test' :: IO String
test' = runMonadPropIO test
