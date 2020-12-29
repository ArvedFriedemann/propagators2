module Control.Combinator.Logics where

import "base" Control.Monad
import "base" Debug.Trace

import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Control.Util

--TODO: does not delete listeners
disjunctForkList :: (Monad m, PropagatorMonad m, Forkable m, BoundedLattice a, Value a) => Cell m a -> [m ()] -> m ()
disjunctForkList _ [] = return ()
disjunctForkList c [m] = do
  fork (\lft -> do
    namedWatch c ("->"++show c) (lft.(write c))
    m
    )
disjunctForkList c mlst = do
  ms <- forM mlst (\m -> do
    rc <- newEmptyCell "rc"
    return (rc, m))
  case ms of
    ((rc1,_):(rc2,_):rms) -> do
      void $ namedWatch rc1 "rc1mult" $ disjunctMultiListener c (rc1:rc2:(fst <$> rms))
      void $ namedWatch rc2 "rc2mult" $ disjunctMultiListener c (rc1:rc2:(fst <$> rms))
      forM_ ms (\(rc, m) -> namedFork "rcf" (\lft -> do
        namedWatch c "c->rc" (lft.(write rc))
        m
        ))
    _ -> error "list should have at least two elements!"


disjunctFork :: (Monad m, PropagatorMonad m, Forkable m, BoundedLattice a, Value a) => Cell m a -> m () -> m () -> m ()
disjunctFork r m1 m2 = disjunctForkList r [m1,m2]


--TODO: does not remove subscriptions
disjunctListener :: (Monad m, PropagatorMonad m, BoundedJoin a, Value a) => Cell m a -> Cell m a -> a -> m ()
disjunctListener r ca b
  | b == bot =  void $ eq r ca
  | otherwise = return ()

--actually observes a list of cells, but as it only needs to be triggered by one it looks like a normal listener linked to one variable
disjunctMultiListener :: (Monad m, PropagatorMonad m, BoundedJoin a, Value a) => Cell m a -> [Cell m a] -> a -> m ()
disjunctMultiListener res cells _ = do
  rds <- mapM (\c -> readCell c >>= \c' -> return (c',c)) cells
  let valids = filter (not.(== bot).fst) rds
  if isSingleton valids
    then void $ eq res (snd $ head valids)
    else return ()
