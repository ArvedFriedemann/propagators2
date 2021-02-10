{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Base where

import "base" Prelude hiding ( read )

import "this" Control.Propagator.Propagator
import "this" Control.Propagator.Class


class (Std i, Value a) => Identifier i a | i -> a

{-|
@
do
  write i a
fixpoint
do
  a' <- read i
  a' >= a
@

@
do
  watch i (Write j)
  write i a
fixpoint
do
  a' <- read i
  a' >= a
@

@
do
  s <- scope
  s' <- inScope scope
  s == s'
@

initially
@
do
  Root <- scope
@
-}
class Monad m => MonadProp m where

    write :: Identifier i a => i -> a -> m i

    read :: Identifier i a => i -> m a

    watch :: (Identifier i a, Propagator m a p) => i -> p -> m i

    watchFixpoint :: Std i => i -> m () -> m ()

    parScoped :: (forall i. Std i => i -> m a) -> m (Maybe a)

    scoped :: Std i => i -> m a -> m a
