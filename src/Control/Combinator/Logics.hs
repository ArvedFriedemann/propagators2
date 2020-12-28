module Control.Combinator.Logics where

import "base" Control.Monad
import "base" Debug.Trace

import "this" Control.Propagator.Class
import "this" Data.Lattice



disjunctFork :: (Monad m, PropagatorMonad m, Forkable m, BoundedLattice a, Value a) => Cell m a -> m () -> m () -> m ()
disjunctFork r m1 m2 = do
  rc1 <- newEmptyCell "rc1"
  rc2 <- newEmptyCell "rc2"
  disjunct rc1 rc2 r
  namedFork "A" $ \lft -> do
    m1
    void . watch r $ \ x -> do
      lft $ write rc1 x
      traceM $ "branch A: "++ show r ++" "++ show x
  namedFork "B" $ \lft -> do
    m2
    void . watch r $ \ x -> do
      lft $ write rc2 x
      traceM $ "branch B: "++ show r ++" "++show x

--If one of the values becomes bot, the output it set equal to the other value
disjunct :: (Monad m, PropagatorMonad m, BoundedLattice a, Value a) => Cell m a -> Cell m a -> Cell m a -> m (Subscriptions m)
disjunct a b r = do
  unsub1 <- watch a (disjunctListener r b)
  unsub2 <- watch b (disjunctListener r a)
  return (unsub1 <> unsub2)

--TODO: does not remove subscriptions
disjunctListener :: forall a m. (Monad m, PropagatorMonad m, BoundedLattice a, Value a) => Cell m a -> Cell m a -> a -> m ()
disjunctListener r ca b = do
  traceM $ "### test " ++ show ca ++ ": " ++ show b ++ " = " ++ show @a bot
  if b == bot
  then traceM ("The thing that's not "++(show ca)++" is bot") >> void $ eq r ca
  else return ()
