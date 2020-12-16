module Data.Constraints.Combinators where

import "this" Data.Lattice
import "this" Control.Propagator


add :: (Num a, Ord a, Meet a, PropagatorMonad m) => Cell m a -> Cell m a -> Cell m a -> m (Subscription m)
add a b c = do
    unsub0 <- link2 a b c (+)
    unsub1 <- link2 c a b (-)
    unsub2 <- link2 c b a (-)
    pure (unsub0 <> unsub1 <> unsub2)
