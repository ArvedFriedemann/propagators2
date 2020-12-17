module Data.Perhaps where

import "this" Data.Lattice


data Perhaps a
    = Unknown
    | Known a
    | Contradiction
  deriving (Eq, Ord, Show)

instance Eq a => Meet (Perhaps a) where
    Unknown /\ a = a
    a /\ Unknown = a
    Known a /\ Known b | a == b = Known a
    _ /\ _ = Contradiction

instance Eq a => BoundedMeet (Perhaps a) where
    top = Unknown

instance Eq a => Join (Perhaps a) where
    Contradiction \/ a = a
    a \/ Contradiction = a
    Known a \/ Known b | a == b = Known a
    _ \/ _ = Unknown

instance Eq a => BoundedJoin (Perhaps a) where
    bot = Contradiction

instance Eq a => Lattice (Perhaps a)
instance Eq a => BoundedLattice (Perhaps a)

deriving via (Monoidal (Perhaps a)) instance Eq a => Semigroup (Perhaps a)
deriving via (Monoidal (Perhaps a)) instance Eq a => Monoid (Perhaps a)
