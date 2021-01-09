{-# LANGUAGE StrictData #-}
module Data.Lattice.WithTop where

import "base" Data.Functor.Classes
import "base" Data.String ( IsString(..) )
import "base" Control.Applicative
import "base" GHC.Exts ( IsList(..) )

import "this" Data.Lattice.Class


data WithTop a
    = SynthTop
    | NotTop a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Eq1 WithTop where
    liftEq _ SynthTop SynthTop = True
    liftEq f (NotTop a) (NotTop b) = f a b
    liftEq _ _ _ = False
instance Ord1 WithTop where
    liftCompare f (NotTop a) (NotTop b) = f a b
    liftCompare _ (NotTop _) _ = GT
    liftCompare _ _ (NotTop _) = LT
    liftCompare _ SynthTop SynthTop = EQ
instance Show1 WithTop where
    liftShowsPrec _ _ _ SynthTop = showString "Top"
    liftShowsPrec f _ d (NotTop a) = f d a
instance IsList a => IsList (WithTop a) where
    type Item (WithTop a) = Item a
    fromList = NotTop . fromList
    toList SynthTop = []
    toList (NotTop a) = toList a
instance IsString a => IsString (WithTop a) where
    fromString = NotTop . fromString

instance Applicative WithTop where
    pure = NotTop
    NotTop f <*> NotTop a = NotTop $ f a
    _ <*> _ = SynthTop
instance Monad WithTop where
    SynthTop >>= _ = SynthTop
    NotTop a >>= f = f a

instance Meet a => Meet (WithTop a) where
    (/\) = liftA2 (/\)
instance HasTop (WithTop a) where
    isTop SynthTop = True
    isTop _ = False
    top = SynthTop
instance Join a => Join (WithTop a) where
    (\/) = liftA2 (\/)
instance HasBot a => HasBot (WithTop a) where
    isBot SynthTop = False
    isBot (NotTop a) = isBot a
    bot = pure bot
instance Meet a => BoundedMeet (WithTop a) where
instance BoundedJoin a => BoundedJoin (WithTop a) where
instance (Meet a, Join a) => Lattice (WithTop a)
instance (Meet a, BoundedJoin a) => BoundedLattice (WithTop a)
