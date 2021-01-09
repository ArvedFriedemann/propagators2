{-# LANGUAGE UndecidableInstances #-}
module Data.Lattice.Domain
    ( Domain
    , pattern Domain
    , pattern Top
    , getDomain
    , liftD2
    , mapD
    ) where

import "base" Data.Functor.Compose
import "base" GHC.Exts ( IsList(..) )

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "this" Data.Lattice.Class
import "this" Data.Lattice.WithTop
import "this" Data.Lattice.HasValue


newtype Domain a = D (WithTop (Set a))
  deriving stock (Show, Read)
  deriving (HasValue, Foldable) via (Compose WithTop Set)
  deriving newtype
    ( Eq, Ord, IsList
    , Meet, Join, HasTop, HasBot
    , BoundedMeet, BoundedJoin
    , Lattice, BoundedLattice)
pattern Domain :: Set a -> Domain a
pattern Domain s = D (NotTop s)
{-# COMPLETE Top, Domain #-}

getDomain :: Domain a -> Set a
getDomain (Domain s) = s
getDomain Top = Set.empty

liftD2 :: Ord c => (a -> b -> c) -> Domain a -> Domain b -> Domain c
liftD2 f (Domain a) (Domain b) = Domain . Set.map (uncurry f) $ Set.cartesianProduct a b
liftD2 _ _ _ = Top

mapD :: Ord b => (a -> b) -> Domain a -> Domain b
mapD f (Domain s) = Domain . Set.map f $ s
mapD _ _ = Top

fromSingleton :: String -> Domain a -> a
fromSingleton _ (Value a) = a
fromSingleton e _ = error $ e ++ ": expected singleton domain"

instance (Ord a, Num a) => Num (Domain a) where
    (+) = liftD2 (+)
    (*) = liftD2 (*)
    (-) = liftD2 (-)
    abs = mapD abs
    signum = mapD signum
    negate = mapD negate
    fromInteger = toValue . fromInteger

instance (Ord a, Fractional a) => Fractional (Domain a) where
    fromRational = toValue . fromRational
    (/) = liftD2 (/)

instance (Ord a, Real a) => Real (Domain a) where
    toRational = toRational . fromSingleton "toRational"

instance (Ord a, Enum a) => Enum (Domain a) where
    toEnum = fromList . pure . toEnum
    fromEnum = fromEnum . fromSingleton "fromEnum"

instance (Ord a, Integral a) => Integral (Domain a) where
    toInteger = toInteger . fromSingleton "toInteger"
    quotRem a b = (quot a b, rem a b)
    quot = liftD2 quot
    rem = liftD2 rem
    div = liftD2 div
    mod = liftD2 mod
    