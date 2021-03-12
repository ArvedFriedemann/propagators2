{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Class where

import "base" Prelude hiding ( read )
import "base" Data.Typeable

import "this" Control.MonadVar.MonadVar (MonadNew, MonadMutate, MonadRead)

import "this" Data.Lattice

class (Eq a, Ord a, Show a, Typeable a) => Std a
class (HasTop a, Meet a, Eq a, Typeable a) => Value a

class Dep a b | a -> b

class (MonadNew m v, MonadRead m v, MonadMutate m v) => MonadVar m v

class MonadPropSimple m v | m -> v where
  readS :: v a -> m a
  writeS ::(Value a) =>  v a -> a -> m ()
  watchS :: v a -> m () -> m ()

class (Monad m) => MonadFork m where
  fork :: m () -> m ()
  forkF :: (Foldable t) => t (m ()) -> m ()
  forkF = foldr (\m t -> fork m >> t) (return ())

class (Monad m', MonadVar m' v, MonadVar m v) => MonadAtomic v m' m | m -> m' v where
  atomically :: m' a -> m a

class Identifier i a | i -> a

data Scope = Scope

class Monad m => MonadProp m v | m -> v where
  read :: v a -> m a
  write :: (Value a) => v a -> a -> m ()
  watch :: (Value a, Std n) => v a -> n -> m () -> m ()

  new :: (Identifier n a, Value a, Std n) => n -> m (v a)

  newScope :: (Identifier n Scope, Std n) => n -> m (v Scope)
  scoped :: v Scope -> m () -> m ()
  parScoped :: m () -> m ()

  watchFixpoint :: m () -> m ()
