{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Class where

import "base" Prelude hiding ( read )

class (Eq a, Ord a, Show a, Typeable a) => Std a
class (BoundedJoin a) => Value a

type Scope = Scope

class Monad m => MonadProp m v where
  read :: (Value a) => v a -> m a
  write :: (Value a) => v a -> a -> m ()
  watch :: (Value a, Std n) => v a -> n -> m () -> m ()

  new :: (Std n) => n -> m (v a)

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
  newScope :: (Std n) => n -> m (v Scope)
  scoped :: v Scope -> m () -> m ()
  parScoped :: m () -> m ()

  watchFixpoint :: m () -> m ()
