module Data.Lattice.HasValue where

import "base" Data.Functor.Compose
import "base" Control.Monad

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "unordered-containers" Data.HashSet ( HashSet )
import "unordered-containers" Data.HashSet qualified as HashSet

import "hashable" Data.Hashable

import "this" Data.Lattice.Class
import "this" Data.Lattice.WithTop
import "this" Data.Lattice.WithBot
import "this" Data.Lattice.WithBounds


class HasValue f a | f -> a where
    fromValue :: f -> Maybe a
    toValue :: a -> f
pattern Value :: HasValue f a => a -> f
pattern Value v <- (fromValue -> Just v)
  where Value v = toValue v


instance HasValue [a] a where
    fromValue (a : _) = Just a
    fromValue _ = Nothing
    toValue = pure
instance HasValue (Maybe a) a where
    fromValue = id
    toValue = Just
instance (HasValue (f (g a)) (g a), HasValue (g a) a) => HasValue (Compose f g a) a where
    fromValue = (fromValue >=> fromValue) . getCompose
    toValue = Compose . toValue . toValue
instance Ord a => HasValue (Set a) a where
    fromValue s | Set.size s == 1 = Set.lookupMax s
    fromValue _ = Nothing
    toValue = Set.singleton
instance Hashable a => HasValue (HashSet a) a where
    fromValue = fromValue . HashSet.toList
    toValue = HashSet.singleton

-- instance

instance HasValue (WithBot a) a where
    fromValue (NotBot a) = Just a
    fromValue _ = Nothing
    toValue = NotBot
{-# COMPLETE Bot, Value :: WithBot #-}

instance HasValue (WithTop a) a where
    fromValue (NotTop a) = Just a
    fromValue _ = Nothing
    toValue = NotTop
{-# COMPLETE Top, Value :: WithTop #-}

instance HasValue (WithBounds a) a where
    fromValue (WithBounds b) = fromValue b
    toValue = WithBounds . toValue
{-# COMPLETE Bot, Value, Top :: WithBounds #-}
