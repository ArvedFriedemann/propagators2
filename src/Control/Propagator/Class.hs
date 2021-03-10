{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Class where

import "base" Prelude hiding ( read )
import "base" Data.Typeable

import "this" Data.Lattice

class (Eq a, Ord a, Show a, Typeable a) => Std a
class (HasTop a, Meet a, Eq a) => Value a

class Dep a b | a -> b

class Monad m => MonadPropSimple m v | m -> v where
  readS :: v a -> m a
  writeS ::(Value a) =>  v a -> a -> m ()
  watchS :: v a -> m () -> m ()

class (Monad m) => MonadFork m where
  fork :: m () -> m ()
  forkF :: (Foldable t) => t (m ()) -> m ()
  forkF = foldr (\m t -> fork m >> t) (return ())


class Identifier i a | i -> a

data Scope = Scope

class Monad m => MonadProp m v | m -> v where
  read :: (Value a) => v a -> m a
  write :: (Value a) => v a -> a -> m ()
  watch :: (Value a, Std n) => v a -> n -> m () -> m ()

  new :: (Identifier n a, Std n) => n -> m (v a)

  --TODO: how to do scopes when we only have normal references?
  --possible solution: have an actual create event for scopes that gives a reference
  --that can be found quicker in a map
  --Hint from : use doubly linked tree, like
  {-
    data Ref a = Ref
    { parent :: IORef (Ref a)
    , value :: IORef a
    , children :: IORef (Map ScopeName (Ref a))
    }
  -}
  newScope :: (Identifier n Scope, Std n) => n -> m (v Scope)
  scoped :: v Scope -> m () -> m ()
  parScoped :: m () -> m ()

  watchFixpoint :: m () -> m ()
