{-# LANGUAGE NoImplicitPrelude #-}
module Control.Combinator.Logics
    ( disjunctFork
    ) where

import "base" Prelude hiding ( read )
import "base" Data.Functor
import "base" Control.Monad

import "this" Control.Propagator.Class
import "this" Control.Propagator.Combinators
import "this" Data.Lattice


data DisjunctFork i a n p = DisjunctFork
    { target :: i
    , propagator :: p
    , index :: n
    } deriving (Eq, Ord, Show)
instance (Std p, Value n, Identifier i a) => Identifier (DisjunctFork i a n p) a

instance (MonadProp m, Identifier i a, Propagator m p n)
         => Forked m (DisjunctFork i a n p) where
    inFork (DisjunctFork i p n) lft = do
        promote lft i
        propagate p n

disjunctFork :: forall i p n a m. 
             ( MonadProp m, Forkable m, Value n, BoundedJoin a, Identifier i a
             , Propagator m p n
             ) => i -> p -> [n] -> m ()
disjunctFork i p ns = dfs `forM_` \df -> do
    df `watch` PropagateWinnner dfs
    fork df
  where
    dfs :: [DisjunctFork i a n p]
    dfs = DisjunctFork i p <$> ns

newtype PropagateWinnner i a n p = PropagateWinnner [DisjunctFork i a n p]
  deriving (Eq, Ord, Show)
instance (MonadProp m, BoundedJoin a, Identifier i a, Propagator m p n)
         => Propagator m (PropagateWinnner i a n p) a where
    propagate (PropagateWinnner forks) _ = do
        fconts <- fmap join . forM forks $ \f -> read f <&> \case
            Bot -> [] 
            _   -> [f]
        case fconts of
            [f] -> target f `eq` f
            _   -> pure ()
