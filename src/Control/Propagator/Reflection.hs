module Control.Propagator.Reflection where

import "base" Data.Typeable

import "this" Control.Propagator.Propagator
import "this" Control.Propagator.Base
import "this" Data.Lattice
import "this" Data.Some


data PropagatorsOf m i = PropagatorsOf i deriving (Eq, Ord, Show)
instance (Typeable m, Identifier i a) => Identifier (PropagatorsOf m i) (Facts (Some (Propagator m a)))
