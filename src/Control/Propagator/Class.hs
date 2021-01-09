module Control.Propagator.Class where

import "base" Data.Typeable

import "this" Data.Lattice
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

class (Std p, Value a) => Propagator m p a where
    propagate :: p -> a -> m ()

instance (Identifier i a, Identifier j a) => Identifier (Either i j) a

class (BoundedMeet a, Ord a, Std a) => Value a
instance (BoundedMeet a, Ord a, Std a) => Value a

class Monad m => MonadProp m where

    write :: Identifier i a => i -> a -> m i

    read :: (Value a, Identifier i a) => i -> m a

    watch :: (Identifier i a, Propagator m p a) => i -> p -> m i

type LiftParent m = forall a. m a -> m a

class Std i => Forked m i where
    inFork :: i -> LiftParent m -> m ()
class Applicative m => Forkable m where
    fork :: Forked m i => i -> m ()
