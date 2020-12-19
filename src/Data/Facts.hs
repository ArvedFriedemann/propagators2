{-# LANGUAGE UndecidableInstances #-}
module Data.Facts where

import "this" Data.Lattice
import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as S

data Fact a
    = Top
    | Atom a
    | Bot
  deriving (Eq, Ord, Show, Functor)

data FactSet a = FSet (Set a) | SBot
  deriving (Eq, Ord, Show)

data LogFact a
    = LTop
    | LAtom a
    | LogFact a :&: LogFact a
    | LogFact a :|: LogFact a
    | LogFact a :=> LogFact a
    | LBot
  deriving (Eq, Ord, Show, Functor)


--TODO: Clean this up!

instance Eq a => Meet (Fact a) where
  Top /\ a = a
  a /\ Top = a
  Bot /\ _ = Bot
  _ /\ Bot = Bot
  a /\ b
    | a == b    = a
    | otherwise = Bot
instance Eq a => BoundedMeet (Fact a) where
    top = Top
instance Eq a => Join (Fact a) where
  Bot \/ a = a
  a \/ Bot = a
  Top \/ _ = Top
  _ \/ Top = Top
  a \/ b
    | a == b    = a
    | otherwise = Bot
instance Eq a => BoundedJoin (Fact a) where
    bot = Bot
instance Eq a => Lattice (Fact a)
instance Eq a => BoundedLattice (Fact a)



instance (Eq a, Ord a) => Meet (FactSet a) where
  SBot /\ _ = SBot
  _ /\ SBot = SBot
  (FSet a) /\ (FSet b) = FSet $ S.union a b
instance (Eq a, Ord a) => BoundedMeet (FactSet a) where
    top = FSet $ S.empty
instance (Eq a, Ord a) => Join (FactSet a) where
  SBot \/ a = a
  a \/ SBot = a
  (FSet a) \/ (FSet b) = FSet $ S.intersection a b
instance (Eq a, Ord a) => BoundedJoin (FactSet a) where
    bot = SBot
instance (Eq a, Ord a) => Lattice (FactSet a)
instance (Eq a, Ord a) => BoundedLattice (FactSet a)



instance Eq a => Meet (LogFact a) where
    LTop /\ a = a
    a /\ LTop = a
    LBot /\ _ = LBot
    _ /\ LBot = LBot
    a /\ b
      | a == b    = a
      | otherwise = a :&: b
instance Eq a => BoundedMeet (LogFact a) where
    top = LTop
instance Eq a => Join (LogFact a) where
    LBot \/ a = a
    a \/ LBot = a
    LTop \/ _ = LTop
    _ \/ LTop = LTop
    a \/ b
      | a == b    = a
      | otherwise = a :|: b
instance Eq a => BoundedJoin (LogFact a) where
    bot = LBot
instance Eq a => Lattice (LogFact a)
instance Eq a => BoundedLattice (LogFact a)
