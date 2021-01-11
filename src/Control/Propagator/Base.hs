{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Base where

import "base" Prelude hiding ( read )

import "this" Control.Propagator.Propagator
import "this" Control.Propagator.Class
import "this" Control.Propagator.Scope


class (Std i, Value a) => Identifier i a | i -> a

class Monad m => MonadProp m where
    
    write :: (Value a, Identifier i a) => i -> a -> m i

    read :: (Value a, Identifier i a) => i -> m a

    watch :: (Value a, Identifier i a, Propagator m p a) => i -> p -> m i

    scope :: m Scope

    inScope :: Scope -> m a -> m a
