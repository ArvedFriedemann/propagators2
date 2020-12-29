module Control.Language.LogLang where

import "base" Control.Monad

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Combinator.Logics
import "this" Control.Propagator.Class

type Clause m = [Cell m (TermSet m)]
type KB m = [Clause m]

splitClause :: Clause m -> ([Cell m (TermSet m)], Cell m (TermSet m))
splitClause cls = (init cls, last cls)

--TODO, WARNING: empty clauses!
simpleKBNetwork :: (Forkable m, PropagatorMonad m) => KB m -> Cell m (TermSet m) -> m ()
simpleKBNetwork kb goal = do
  disjunctForkList goal [eq post goal >>
                         forM_ pres (simpleKBNetwork kb)
            |(splitClause -> (pres,post)) <- kb]
