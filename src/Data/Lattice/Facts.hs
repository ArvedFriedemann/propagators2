{-# LANGUAGE StrictData #-}
module Data.Lattice.Facts
    ( Facts
    , pattern Facts
    , pattern Bot
    , getFacts
    ) where

import "base" Data.Functor.Compose
import "base" Data.Monoid ( Dual(..) )
import "base" Control.Monad
import "base" GHC.Exts ( IsList(..) )

import "unordered-containers" Data.HashSet ( HashSet )
import "unordered-containers" Data.HashSet qualified as HashSet

import "hashable" Data.Hashable

import "this" Data.Lattice.Class
import "this" Data.Lattice.HasValue
import "this" Data.Lattice.WithBot


newtype Facts a = Fs (WithBot (HashSet a))
  deriving newtype (Eq, Ord, Hashable)
  deriving Foldable via (Compose WithBot HashSet)
  deriving
    ( HasTop, HasBot
    , Meet, BoundedMeet
    , Join, BoundedJoin
    , Lattice, BoundedLattice
    ) via (Compose WithBot Dual (HashSet a))
pattern Facts :: HashSet a -> Facts a
pattern Facts s = Fs (NotBot s)
{-# COMPLETE Bot, Facts #-}
getFacts :: Facts a -> HashSet a
getFacts (Facts s) = s
getFacts Bot = HashSet.empty

instance Hashable a => HasValue (Facts a) a where
    fromValue (Fs x) = fromValue >=> fromValue $ x
    toValue = Fs . NotBot . toValue
instance (Show a, Eq a, Hashable a) => Show (Facts a) where
    showsPrec _ Top = showString "Top"
    showsPrec _ Bot = showString "Bot"
    showsPrec d (Facts a)
        = showParen (d >= 10)
        $ showString "Facts "
        . showsPrec 11 a

instance (Hashable a, Eq a) => IsList (Facts a) where
    type Item (Facts a) = a
    toList Bot = []
    toList (Facts s) = toList s
    fromList = Facts . fromList
