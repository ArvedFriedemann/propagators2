module Control.Language.LogLang where

import "base" Control.Monad

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Combinator.Logics
import "this" Control.Propagator.Class
import "this" Data.Lattice

type Clause m = [Cell m (TermSet m)]
--clauses need to memorise their universal variables
type KB m = [([TermConst], Clause m)]

splitClause :: Clause m -> ([Cell m (TermSet m)], Cell m (TermSet m))
splitClause cls = (init cls, last cls)

refreshClause :: (PropagatorMonad m) => ([TermConst], Clause m) -> m (Clause m)
refreshClause (binds, trms) = do
  forM trms $ \t -> do
    vartbl <- forM binds (\b -> do
      c <- newEmptyCell "fresh"
      return (b,c))
    cpy <- newEmptyCell "freshTerm"
    refreshVarsTbl vartbl t cpy
    return cpy

--TODO, WARNING: empty clauses!
simpleKBNetwork :: (Forkable m, PropagatorMonad m) => KB m -> Cell m (TermSet m) -> m ()
simpleKBNetwork kb goal = do
  g <- readCell goal
  unless (g==bot) $
    disjunctForkList goal [do
      (splitClause -> (pres, post)) <- refreshClause cls
      eq post goal
      forM_ pres (void . recursiveCall . simpleKBNetwork kb)
      |cls <- kb]
