{-# LANGUAGE UndecidableInstances #-}
module Data.Lattice.Domain
    ( Domain
    , pattern Domain
    , pattern Top
    , getDomain
    ) where

import "base" Data.Functor.Compose
import "base" GHC.Exts ( IsList(..) )

import "unordered-containers" Data.HashSet ( HashSet )
import "unordered-containers" Data.HashSet qualified as HashSet

import "hashable" Data.Hashable

import "this" Data.Lattice.Class
import "this" Data.Lattice.WithTop
import "this" Data.Lattice.HasValue


newtype Domain a = D (WithTop (HashSet a))
  deriving stock (Show, Read)
  deriving Foldable via (Compose WithTop HashSet)
  deriving newtype
    ( Eq, Ord, IsList
    , Hashable
    , Meet, Join, HasTop, HasBot
    , BoundedMeet, BoundedJoin
    , Lattice, BoundedLattice)
pattern Domain :: HashSet a -> Domain a
pattern Domain s = D (NotTop s)
{-# COMPLETE Top, Domain #-}

getDomain :: Domain a -> HashSet a
getDomain (Domain s) = s
getDomain Top = HashSet.empty

instance Hashable a => HasValue (Domain a) a where
    fromValue = fromValue . getDomain
    toValue = D . NotTop . HashSet.singleton
