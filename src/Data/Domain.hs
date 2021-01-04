{-# LANGUAGE UndecidableInstances #-}
module Data.Domain where

import "base" GHC.Exts ( IsList(..) )

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "this" Data.Lattice


newtype Domain a = Domain (WithTop (Set a))
  deriving newtype (Eq, Ord, IsList, Meet, Join, BoundedMeet, BoundedJoin, Lattice, BoundedLattice)
  deriving stock (Show, Read)
instance HasValue Domain where
    fromValue (Domain (Value (Value v))) = pure v
    fromValue _ = Nothing
    toValue = Domain . Value . Set.singleton

binOp :: Ord a => (a -> a -> a) -> Domain a -> Domain a -> Domain a
binOp f (Domain (Value a)) (Domain (Value b)) = Domain . Value . Set.map (uncurry f) $ Set.cartesianProduct a b
binOp _ _ _ = Top

mapD :: Ord b => (a -> b) -> Domain a -> Domain b
mapD f (Domain (Value s)) = Domain . Value . Set.map f $ s
mapD _ _ = Top

fromSingleton :: Ord a => String -> Domain a -> a
fromSingleton _ (Value a) = a
fromSingleton e _ = error $ e ++ ": expected singleton domain"

singleton :: Ord a => a -> Domain a
singleton = Domain . Value . Set.singleton

instance (Ord a, Num a) => Num (Domain a) where
    (+) = binOp (+)
    (*) = binOp (*)
    (-) = binOp (-)
    abs = mapD abs
    signum = mapD signum
    negate = mapD negate
    fromInteger = singleton . fromInteger

instance (Ord a, Fractional a) => Fractional (Domain a) where
    fromRational = singleton . fromRational
    (/) = binOp (/)

instance (Ord a, Real a) => Real (Domain a) where
    toRational = toRational . fromSingleton "toRational"

instance (Ord a, Enum a) => Enum (Domain a) where
    toEnum = fromList . pure . toEnum
    fromEnum = fromEnum . fromSingleton "fromEnum"

instance (Ord a, Integral a) => Integral (Domain a) where
    toInteger = toInteger . fromSingleton "toInteger"
    quotRem a b = (quot a b, rem a b)
    quot = binOp quot
    rem = binOp rem
    div = binOp div
    mod = binOp mod
    