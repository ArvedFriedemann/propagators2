{-# LANGUAGE DefaultSignatures #-}
module Data.Num1
    ( Num1(..)
    , LiftNum1(..)
    ) where

import "base" Control.Applicative ( liftA2 )


class Num1 f where
    -- | if you want to provide another Applicative instance
    fromA :: a -> f a
    default fromA :: Applicative f => a -> f a
    fromA = pure

    -- | if you want to provide another Applicative instance
    liftBinOp :: (a -> a -> a) -> f a -> f a -> f a
    default liftBinOp :: Applicative f => (a -> a -> a) -> f a -> f a -> f a
    liftBinOp = liftA2

    -- | if you want to provide another Functor implementation
    liftOp :: (a -> a) -> f a -> f a
    default liftOp :: Functor f => (a -> a) -> f a -> f a
    liftOp = fmap


newtype LiftNum1 f a = LiftNum
    { getNum :: f a
    }
  deriving stock (Show, Read, Foldable, Traversable)
  deriving newtype (Eq, Ord, Functor, Num1)

instance (Num a, Num1 f) => Num (LiftNum1 f a) where
    fromInteger = fromA . fromInteger
    (+) = liftBinOp (+)
    (*) = liftBinOp (*)
    (-) = liftBinOp (-)
    abs = liftOp abs
    signum = liftOp signum
    negate = liftOp negate
