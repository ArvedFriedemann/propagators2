module Control.Combinator.Logics
    ( disjunctFork
    ) where

import "base" Control.Monad

import "this" Control.Propagator.Class
import "this" Control.Propagator.Combinators
import "this" Data.Lattice


data DisjunctFork i = Rc Int i deriving (Eq, Ord, Show)
instance Identifier i a => Identifier (DisjunctFork i) a

disjunctFork :: (MonadProp m, Forkable m, BoundedJoin a, Identifier i a) => i -> [m ()] -> m ()
disjunctFork r = sequence_ . zipWith disjunctFork' [Rc i r | i <- [0..]]
  where  
    disjunctFork' i m = do
        watch i ("disjunct" :: String, i) (disjunctListener r i)
        fork ("disjunct" :: String, i) $ \lft -> watch r i (lft . write i) >> m

disjunctListener :: (MonadProp m, BoundedJoin a, Identifier i a) => i -> DisjunctFork i -> a -> m ()
disjunctListener r ca b
    | b == bot  = void $ eq r ca
    | otherwise = pure ()
