{-# LANGUAGE NoImplicitPrelude #-}
module Control.Combinator.Logics
    ( disjunctFork
    ) where

import "base" Prelude hiding ( read )
import "base" Data.Functor
import "base" Control.Monad
import "base" Debug.Trace

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
disjunctFork goal name ms = djfs `forM_` \(djf, m) -> do
    watch djf . PropagateWinner . fmap fst $ djfs
    scoped djf $ \s -> do
        push s goal djf
        m
  where
    djfs :: [(DisjunctFork i j, m ())]
    djfs = zipWith (\n m -> (DisjunctFork name goal n, m) ) [0..] ms

newtype PropagateWinner i j = PropagateWinner [DisjunctFork i j]
  deriving (Eq, Ord, Show)
instance (Std j, MonadProp m, Value a, BoundedJoin a, Identifier i a)
         => Propagator m a (PropagateWinner i j) where
    propagate (PropagateWinner forks) _ = do
        traceM $ "PropagateWinner"
        fx <- forM forks read
        traceM $ show fx

        fconts <- fmap join . forM forks $ \f -> read f <&> \case
            Bot -> []
            _   -> [f]
        case fconts of
            [f] -> do
                traceM "U are a Wina"
                target f `eq` f
            _   -> pure ()
