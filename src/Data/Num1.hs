{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Num1
    ( Num1(..)
    , LiftNum1(..)
    , ApplicativeOf(..)
    ) where

import "base" Control.Applicative ( liftA2 )

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set


class Num1 f c | f -> c where
    fromA :: c a => a -> f a
    liftBinOp :: c a => (a -> a -> a) -> f a -> f a -> f a
    liftOp :: c a => (a -> a) -> f a -> f a

newtype ApplicativeOf f a = AppOf
    { getF :: f a
    }
  deriving stock (Show, Read, Foldable, Traversable)
  deriving newtype (Eq, Ord, Functor, Applicative)

class Any (a :: k)
instance Any (a :: k)

instance Applicative f => Num1 (ApplicativeOf f)  Any where
    fromA = pure
    liftBinOp = liftA2
    liftOp = fmap

newtype LiftNum1 f a = LiftNum
    { getNum :: f a
    }
  deriving stock (Show, Read, Foldable, Traversable)
  deriving newtype (Eq, Ord, Functor, Applicative)

instance Num1 f c => Num1 (LiftNum1 f) c where
    fromA = LiftNum . fromA
    liftBinOp f (LiftNum a) (LiftNum b) = LiftNum $ liftBinOp f a b
    liftOp f = LiftNum . liftOp f . getNum

instance (Num a, Num1 f c, c a) => Num (LiftNum1 f a) where
    fromInteger = fromA . fromInteger
    (+) = liftBinOp (+)
    (*) = liftBinOp (*)
    (-) = liftBinOp (-)
    abs = liftOp abs
    signum = liftOp signum
    negate = liftOp negate


instance Num1 Set Ord where
    fromA = Set.singleton
    liftBinOp f a b = Set.map (uncurry f) $ Set.cartesianProduct a b
    liftOp = Set.map

deriving via (LiftNum1 Set a) instance (Ord a, Num a) => Num (Set a)
