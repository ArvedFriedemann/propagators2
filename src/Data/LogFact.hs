{-# LANGUAGE UndecidableInstances #-}
module Data.LogFact where

import "this" Data.Lattice


data LogFact a
    = LTop
    | LAtom a
    | LogFact a :&: LogFact a
    | LogFact a :|: LogFact a
    | LogFact a :=> LogFact a
    | LBot
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


instance HasTop (LogFact a) where
    isTop LTop = True
    isTop _ = False
    top = LTop
instance HasBot (LogFact a) where
    isBot LBot = True
    isBot _ = False
    bot = LBot
instance HasValue (LogFact a) a where
    fromValue (LAtom a) = Just a
    fromValue _ = Nothing
    toValue = LAtom
instance Eq a => Meet (LogFact a) where
    a /\ b
      | isTop a = b
      | isTop b = a
      | isBot a || isBot b = Bot
      | a == b    = a
      | otherwise = a :&: b
instance Eq a => BoundedMeet (LogFact a)
instance Eq a => Join (LogFact a) where
    a \/ b
      | isBot a = b
      | isBot b = b
      | isTop a || isTop b = Top
      | a == b    = a
      | otherwise = a :|: b
instance Eq a => BoundedJoin (LogFact a)
instance Eq a => Lattice (LogFact a)
instance Eq a => BoundedLattice (LogFact a)
