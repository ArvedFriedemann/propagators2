module Control.Propagator.Class where

import "base" Data.Typeable

import "this" Data.Lattice


class (Ord a, Typeable a, Show a) => Std a
instance (Ord a, Typeable a, Show a) => Std a

class (BoundedMeet a, BoundedJoin a, Std a) => Value a
instance (BoundedMeet a, BoundedJoin a, Std a) => Value a
