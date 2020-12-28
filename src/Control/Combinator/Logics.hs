module Control.Combinator.Logics where

import "base" Control.Monad
import "base" Debug.Trace

import "this" Control.Propagator.Class
import "this" Data.Lattice



disjunctFork :: (Monad m, PropagatorMonad m, Forkable m, BoundedLattice a, Value a) => Cell m a -> m () -> m () -> m ()
disjunctFork r m1 m2 = do
  rc1 <- newEmptyCell'
  rc2 <- newEmptyCell'
  disjunct rc1 rc2 r
  fork (\lft -> do
    watch r (lft . write rc1)
    m1
    )
  fork (\lft -> do
    watch r (lft . write rc2)
    m2
    )

--If one of the values becomes bot, the output it set equal to the other value
disjunct :: (Monad m, PropagatorMonad m, BoundedLattice a, Value a) => Cell m a -> Cell m a -> Cell m a -> m (Subscriptions m)
disjunct a b r = do
  unsub1 <- watch a (disjunctListener r b)
  unsub2 <- watch b (disjunctListener r a)
  return (unsub1 <> unsub2)

--TODO: does not remove subscriptions
disjunctListener :: (Monad m, PropagatorMonad m, BoundedJoin a, Value a) => Cell m a -> Cell m a -> a -> m ()
disjunctListener r ca b
  | b == bot =  void $ eq r ca
  | otherwise = return ()
