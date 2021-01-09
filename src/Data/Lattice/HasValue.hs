module Data.Lattice.HasValue where

import "base" Data.Functor.Compose
import "base" Control.Monad

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "this" Data.Lattice.Class
import "this" Data.Lattice.WithTop
import "this" Data.Lattice.WithBot
import "this" Data.Lattice.WithBounds


class HasValue f where
    fromValue :: f a -> Maybe a
    toValue :: a -> f a
pattern Value :: HasValue f => a -> f a
pattern Value v <- (fromValue -> Just v)
  where Value v = toValue v


instance HasValue Maybe where
    fromValue = id
    toValue = Just
instance (HasValue f, HasValue g) => HasValue (Compose f g) where
    fromValue = (fromValue >=> fromValue) . getCompose
    toValue = Compose . toValue . toValue
instance HasValue Set where
    fromValue s | Set.size s == 1 = Set.lookupMax s
    fromValue _ = Nothing
    toValue = Set.singleton

-- instance

instance HasValue WithBot where
    fromValue (NotBot a) = Just a
    fromValue _ = Nothing
    toValue = NotBot
{-# COMPLETE Bot, Value :: WithBot #-}

instance HasValue WithTop where
    fromValue (NotTop a) = Just a
    fromValue _ = Nothing
    toValue = NotTop
{-# COMPLETE Top, Value :: WithTop #-}

deriving newtype instance HasValue WithBounds
{-# COMPLETE Bot, Value, Top :: WithBounds #-}
