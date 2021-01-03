module Control.Propagator.Class where

import "base" Data.Maybe
import "base" Data.Functor
import "base" Data.Typeable

import "this" Data.Lattice
import "this" Data.Facts


class (Ord a, Typeable a, Show a) => Std a
instance (Ord a, Typeable a, Show a) => Std a

data SomeStd where
    SomeStd :: Std a => a -> SomeStd

class (Std i, Value a) => Identifier i a | i -> a

instance (Identifier i a, Identifier j a) => Identifier (Either i j) a

class (Meet a, Ord a, Std a) => Value a
instance (Meet a, Ord a, Std a) => Value a

class (BoundedMeet a, Value a) => BoundedValue a
instance (BoundedMeet a, Value a) => BoundedValue a

class Monad m => MonadProp m where

    write :: Identifier i a => i -> a -> m ()

    read :: (BoundedValue a, Identifier i a) => i -> m a

    watch :: (Identifier i a, Std j) => i -> j -> (a -> m ()) -> m ()

type LiftParent m = forall a. m a -> m a

class Applicative m => Forkable m where
    fork :: Std i => i -> (LiftParent m -> m ()) -> m ()
