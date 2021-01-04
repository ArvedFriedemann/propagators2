{-# LANGUAGE NoImplicitPrelude #-}
module Control.Combinator.Logics
    ( disjunctFork
    ) where

import "base" Prelude hiding ( read )

import "base" Control.Monad
import "base" Debug.Trace

import "this" Control.Propagator.Class
import "this" Control.Propagator.Combinators
import "this" Data.Lattice
import "this" Control.Util

{-
--TODO: does not delete listeners
disjunctForkList :: (Monad m, MonadProp m, Forkable m, BoundedLattice a, Value a) => Cell m a -> [m ()] -> m ()
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
-}

data DisjunctFork i = Rc Int i deriving (Eq, Ord, Show)
instance Identifier i a => Identifier (DisjunctFork i) a

--WARNING, TODO: The whole disjunctFork needs a reason on its own
disjunctFork :: (MonadProp m, Forkable m, BoundedJoin a, Identifier i a) => i -> [m ()] -> m ()
disjunctFork _ [] = pure ()
disjunctFork tg [m] =
  fork ("disjunct" :: String, Rc 0 tg) (\lft -> do
      watch tg ("disjunct" :: String, Rc 0 tg) (void.lft.(write tg))
      m
    )
disjunctFork tg ms = do --ms has at least 2 elements
    watch (Rc 0 tg) ()
      (disjunctForkMultiListener tg rcs)
    watch (Rc 1 tg) ()
      (disjunctForkMultiListener tg rcs)
    sequence_ $ zipWith disjunctFork' rcs ms
  where rcs = [Rc i tg | i <- [0..]]
        disjunctFork' i m = do
            fork ("disjunct" :: String, i) $ \lft ->
              watch i () (lft . write i) >> m

disjunctForkMultiListener :: (MonadProp m, BoundedJoin a, Identifier i a) => i -> [DisjunctFork i] -> a -> m ()
disjunctForkMultiListener _ [] _ = pure ()
disjunctForkMultiListener tg forks _ = do
  fconts <- mapM read forks
  let
    fctf = zip fconts forks
    sucf = filter ((/= bot).fst) fctf
    in if isSingleton sucf
        then eq tg (snd $ head sucf)
        else pure ()

{-
disjunctFork r = sequence_ . zipWith disjunctFork' [Rc i r | i <- [0..]]
  where
    disjunctFork' i m = do
        watch i ("disjunct" :: String, i) (disjunctListener r i)
        fork ("disjunct" :: String, i) $ \lft -> watch r i (lft . write i) >> m

disjunctListener :: (MonadProp m, BoundedJoin a, Identifier i a) => i -> DisjunctFork i -> a -> m ()
disjunctListener r ca b
    | b == bot  = void $ eq r ca
    | otherwise = pure ()
-}
