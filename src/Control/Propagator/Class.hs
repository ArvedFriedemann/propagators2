module Control.Propagator.Class where

import "base" Data.Typeable

import "this" Data.Lattice


class (Ord a, Typeable a, Show a) => Std a
instance (Ord a, Typeable a, Show a) => Std a

class (BoundedLattice a, Std a) => Value a
instance (BoundedLattice a, Std a) => Value a
