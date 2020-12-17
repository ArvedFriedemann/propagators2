{-# LANGUAGE UndecidableInstances #-}
module Data.Facts where

import "this" Data.Lattice

data Fact a
    = Top
    | Atom a
    | Fact a :&: Fact a
    | Fact a :|: Fact a
    | Fact a :=> Fact a
    | Bot
  deriving (Eq, Ord, Show, Functor)

{-

Cell (Fact a) := {a & b}

"a & b" Cell (Set Bool) := {True}

a = Cell (Set Bool) := {True}
b = Cell (Set Bool) := {True}

"a & b" -(&)> [a, b]

link a b "a & b" (do linking magic)

-}

instance Eq a => Meet (Fact a) where
    Top /\ a = a
    a /\ Top = a
    Bot /\ _ = Bot
    _ /\ Bot = Bot
    a /\ b
      | a == b    = a
      | otherwise = a :&: b
instance Eq a => BoundedMeet (Fact a) where
    top = Top
instance Eq a => Join (Fact a) where
    Bot \/ a = a
    a \/ Bot = a
    Top \/ _ = Top
    _ \/ Top = Top
    a \/ b
      | a == b    = a
      | otherwise = a :|: b
instance Eq a => BoundedJoin (Fact a) where
    bot = Bot
instance Eq a => Lattice (Fact a)
instance Eq a => BoundedLattice (Fact a)
