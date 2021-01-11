{-# LANGUAGE NoImplicitPrelude #-}
module Control.Combinator.Logics
    ( disjunctFork
    ) where

import "base" Prelude hiding ( read )
import "base" Data.Functor
import "base" Control.Monad

import "this" Control.Propagator
import "this" Data.Lattice


data DisjunctFork i a n p = DisjunctFork
    { target :: i
    , propagator :: p
    , index :: n
    } deriving (Eq, Ord, Show)
instance (Std p, Std n, Identifier i a) => Identifier (DisjunctFork i a n p) a

disjunctFork :: forall i p n a m. 
             ( MonadProp m
             , BoundedJoin a, Value a, Identifier i a
             , Propagator m p n
             ) => i -> p -> [n] -> m ()
disjunctFork i p ns = dfs `forM_` \df -> do
    watch df $ PropagateWinner dfs
    scoped df $ \s -> do
        push s (target df) df
        propagate p . index $ df
  where
    dfs :: [DisjunctFork i a n p]
    dfs = DisjunctFork i p <$> ns

newtype PropagateWinner i a n p = PropagateWinner [DisjunctFork i a n p]
  deriving (Eq, Ord, Show)
instance (MonadProp m, Value a, BoundedJoin a, Identifier i a, Propagator m p n)
         => Propagator m (PropagateWinner i a n p) a where
    propagate (PropagateWinner forks) _ = do
        fconts <- fmap join . forM forks $ \f -> read f <&> \case
            Bot -> [] 
            _   -> [f]
        case fconts of
            [f] -> target f `eq` f
            _   -> pure ()
