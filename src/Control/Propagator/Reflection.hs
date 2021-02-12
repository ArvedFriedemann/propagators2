module Control.Propagator.Reflection where

import "base" Data.Typeable
import "base" GHC.Generics

import "hashable" Data.Hashable

import "this" Control.Propagator.Propagator
import "this" Control.Propagator.Base
import "this" Data.Lattice
import "this" Data.Some


newtype PropagatorsOf m i = PropagatorsOf i deriving (Eq, Ord, Show, Generic, Hashable)
instance (Typeable m, Identifier i a) => Identifier (PropagatorsOf m i) (Facts (Some (Propagator m a)))


data Fixpoint = Fixpoint
  deriving (Show, Ord, Eq, Generic)
instance Hashable Fixpoint
instance Identifier Fixpoint ()
