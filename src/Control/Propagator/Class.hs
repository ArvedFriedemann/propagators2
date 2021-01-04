module Control.Propagator.Class where

import "base" Data.Typeable

import "this" Data.Lattice

import "this" Data.Facts
import "this" Data.Typed



class (Ord a, Typeable a, Show a) => Std a
instance (Ord a, Typeable a, Show a) => Std a

data SomeStd where
    SomeStd :: Std a => a -> SomeStd
deriving instance Show SomeStd
instance Eq SomeStd where
    a == b = compare a b == EQ
instance Ord SomeStd where
    SomeStd a `compare` SomeStd b = compareTyped a b


class (Std i, Value a) => Identifier i a | i -> a

instance (Identifier i a, Identifier j a) => Identifier (Either i j) a

class (BoundedMeet a, Ord a, Std a) => Value a
instance (BoundedMeet a, Ord a, Std a) => Value a

class Monad m => MonadProp m where

    write :: Identifier i a => i -> a -> m i

    read :: (Value a, Identifier i a) => i -> m a

    watch :: (Identifier i a, Std j) => i -> j -> (a -> m x) -> m i

type LiftParent m = forall a. m a -> m a

class Applicative m => Forkable m where
    fork :: Std i => i -> (LiftParent m -> m x) -> m ()
