{-# LANGUAGE NoImplicitPrelude #-}
module Control.Combinator.Logics
    ( disjunctFork
    ) where

import "base" Prelude hiding ( read )
import "base" Data.Functor
import "base" Control.Monad
import "base" Debug.Trace
import "base" Data.Typeable

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
disjunctFork = undefined


disjunctForkDestr :: forall i j a m.
             ( MonadProp m
             , Typeable m
             , BoundedJoin a, Identifier i a
             , Std j
             ) => i -> j -> [(m (),m ())] -> m ()
disjunctForkDestr goal name ms = djfs `forM_` \(djf, (constr , _)) -> do
    watch djf $ PropagateWinner djfsDestr
    scoped djf $ \s -> do
        push s goal djf
        constr
  where
    djfs :: [(DisjunctFork i j, (m (), m ()))]
    djfs = zipWith (\n m -> (DisjunctFork name goal n, m) ) [0..] ms
    djfsDestr :: [(DisjunctFork i j, m ())]
    djfsDestr = map (\(x,(_,z)) -> (x, z)) djfs

newtype PropagateWinner i j m = PropagateWinner [(DisjunctFork i j, m ())]
  --deriving (Eq, Ord, Show)
instance (Eq i, Eq j) => Eq (PropagateWinner i j m) where
  (PropagateWinner a) == (PropagateWinner b) = (fst <$> a) == (fst <$> b)
instance (Ord i, Ord j) => Ord (PropagateWinner i j m) where
  compare (PropagateWinner a) (PropagateWinner b) = compare (fst <$> a) (fst <$> b)
instance (Show i, Show j) => Show (PropagateWinner i j m) where
  show (PropagateWinner a) = show (fst <$> a)

instance (Std j, Typeable m, MonadProp m, Value a, BoundedJoin a, Identifier i a)
         => Propagator m a (PropagateWinner i j m) where
    propagate (PropagateWinner forks) _ = do

        fconts <- fmap join . forM forks $ \(f,m) -> read f <&> \case
            Bot -> []
            _   -> [(f,m)]
        case fconts of
            [(f,m)] -> do
                target f `eq` f
                m
            _   -> pure ()
