{-# LANGUAGE UndecidableInstances #-}
module Data.Domain where

import "base" GHC.Exts ( IsList(..) )

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "this" Data.Lattice


newtype Domain a = Domain (Set a)
  deriving newtype (Eq, Ord, IsList, Meet, Join, BoundedMeet, BoundedJoin, Lattice, BoundedLattice)
  deriving stock (Show, Read)

pattern AsList :: IsList l => [Item l] -> l
pattern AsList l <- (toList -> l)
  where AsList l = fromList l
{-# COMPLETE AsList :: Domain #-}
{-# COMPLETE AsList :: Set #-}
{-# COMPLETE AsList :: [] #-}

binOp :: Ord a => (a -> a -> a) -> Domain a -> Domain a -> Domain a
binOp f (Domain a) (Domain b) = Domain . Set.map (uncurry f) $ Set.cartesianProduct a b

mapD :: Ord b => (a -> b) -> Domain a -> Domain b
mapD f (Domain s) = Domain . Set.map f $ s

fromSingleton :: Ord a => String -> Domain a -> a
fromSingleton _ (AsList [a]) = a
fromSingleton e (AsList _) = error $ e ++ ": expected singleton domain"

instance (Ord a, Num a) => Num (Domain a) where
    (+) = binOp (+)
    (*) = binOp (*)
    (-) = binOp (-)
    abs = mapD abs
    signum = mapD signum
    negate = mapD negate
    fromInteger = Domain . Set.singleton . fromInteger

instance (Ord a, Fractional a) => Fractional (Domain a) where
    fromRational = Domain . Set.singleton . fromRational
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
    