{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Base where

import "base" Prelude hiding ( read )

import "this" Control.Propagator.Propagator
import "this" Control.Propagator.Class
import "this" Control.Propagator.Scope


class (Std i, Value a) => Identifier i a | i -> a

class Monad m => MonadProp m where
    
    write :: Identifier i a => i -> a -> m i

    read :: Identifier i a => i -> m a

    watch :: (Identifier i a, Propagator m a p) => i -> p -> m i

    scope :: m Scope

    inScope :: Scope -> m a -> m a
