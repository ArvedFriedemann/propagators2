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

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "this" Data.Lattice.Class
import "this" Data.Lattice.HasValue
import "this" Data.Lattice.WithBot


newtype Facts a = Fs (WithBot (Set a))
  deriving newtype (Eq, Ord)
  deriving Foldable via (Compose WithBot Set)
  deriving
    ( HasTop, HasBot
    , Meet, BoundedMeet
    , Join, BoundedJoin
    , Lattice, BoundedLattice
    ) via (Compose WithBot Dual (Set a))
pattern Facts :: Set a -> Facts a
pattern Facts s = Fs (NotBot s)
{-# COMPLETE Bot, Facts #-}
getFacts :: Facts a -> Set a
getFacts (Facts s) = s
getFacts Bot = Set.empty

instance HasValue Facts where
    fromValue (Fs x) = fromValue >=> fromValue $ x
    toValue = Fs . NotBot . toValue
instance (Show a, Ord a) => Show (Facts a) where
    showsPrec _ Top = showString "Top"
    showsPrec _ Bot = showString "Bot"
    showsPrec d (Facts a)
        = showParen (d >= 10)
        $ showString "Facts "
        . showsPrec 11 a

instance Ord a => IsList (Facts a) where
    type Item (Facts a) = a
    toList Bot = []
    toList (Facts s) = toList s
    fromList = Facts . fromList
