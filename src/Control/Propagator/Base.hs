{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Base where

import "base" Prelude hiding ( read )
import "base" Debug.Trace

import "this" Control.Propagator.Propagator
import "this" Control.Propagator.Class
import "this" Control.Propagator.Scope


class (Std i, Value a) => Identifier i a | i -> a

data Fixpoint = Fixpoint
  deriving (Show, Ord, Eq)
instance Identifier Fixpoint ()

class Monad m => MonadProp m where

    write :: Identifier i a => i -> a -> m i

    read :: Identifier i a => i -> m a

    watch :: (Identifier i a, Propagator m a p) => i -> p -> m i

    scope :: m Scope

    inScope :: Scope -> m a -> m a

    liftParent :: m a -> m a
    liftParent m = do
      s' <- scope

      case s' of
        (s :/ _) -> inScope s m
        _ -> error "Current scope should have a parent!"

    --takes something with a lift function into the fork and operates in in parent
    fromParent :: ((m a -> m a) -> m a) -> m a
    fromParent mf = do
      s <- scope
      liftParent $ mf (inScope s)
