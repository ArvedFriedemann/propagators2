{-# LANGUAGE NoImplicitPrelude #-}
module Control.Combinator.Logics
    ( disjunctFork
    ) where

import "base" Prelude hiding ( read )
import "base" Data.Functor
import "base" Control.Monad

import "this" Control.Propagator
import "this" Data.Lattice


data DisjunctFork i j = DisjunctFork
    { name :: j
    , target :: i
    , index :: Int
    } deriving (Eq, Ord, Show)
instance (Std j, Identifier i a) => Identifier (DisjunctFork i j) a

disjunctFork :: forall i j a m. 
             ( MonadProp m
             , BoundedJoin a, Identifier i a
             , Std j
             ) => i -> j -> [m ()] -> m ()
disjunctFork i j ns = dfs `forM_` \(df, m) -> do
    watch df . PropagateWinner . fmap fst $ dfs
    scoped df $ \s -> do
        push s (target df) df
        m
  where
    dfs :: [(DisjunctFork i j, m ())]
    dfs = zipWith (\n m -> (DisjunctFork j i n, m) ) [0..] ns

newtype PropagateWinner i j = PropagateWinner [DisjunctFork i j]
  deriving (Eq, Ord, Show)
instance (Std j, MonadProp m, Value a, BoundedJoin a, Identifier i a)
         => Propagator m a (PropagateWinner i j) where
    propagate (PropagateWinner forks) _ = do
        fconts <- fmap join . forM forks $ \f -> read f <&> \case
            Bot -> [] 
            _   -> [f]
        case fconts of
            [f] -> target f `eq` f
            _   -> pure ()
