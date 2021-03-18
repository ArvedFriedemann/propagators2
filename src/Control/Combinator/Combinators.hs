module Control.Combinator.Combinators where

import "this" Control.Propagator.Class
import "this" Data.Lattice

import "base" Data.Typeable
import "base" Control.Monad

eq :: (MonadProp m v scope, Value a) => v a -> v a -> m ()
eq p1 p2 = dirEq p1 p2 >> dirEq p2 p1

data DirEq = DirEq
  deriving (Show, Eq, Ord)

dirEq :: (MonadProp m v scope, Value a) => v a -> v a -> m ()
dirEq p1 p2 = watch' p1 DirEq (\v -> write p2 v)

eqAll :: (MonadProp m v scope, Value a, Foldable t) => t (v a) -> m ()
eqAll t = maybe (pure ()) void $ foldr eqAll' Nothing t
  where
    eqAll' i Nothing  = Just (pure i)
    eqAll' i (Just j) = Just (j >>= eq i >> pure i)

data PropBot = PropBot
  deriving (Show, Eq, Ord)

propBot :: (MonadProp m v scope, Value a, Value b, HasBot a, HasBot b) =>
  v a -> v b -> m ()
propBot p1 p2 = watch' p1 PropBot (\v -> when (isBot v) $ write p2 bot)




--
