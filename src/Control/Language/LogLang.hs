{-# LANGUAGE NoImplicitPrelude #-}

module Control.Language.LogLang where

import "base" Prelude hiding ( read )
import "base" Control.Monad

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Combinator.Logics
import "this" Control.Propagator.Class
import "this" Control.Propagator.Combinators
import "this" Data.Lattice

type Clause i = [i]
--clauses need to memorise their universal variables
type KB i = [([TermConst], Clause i)]

splitClause :: Clause i -> ([i], i)
splitClause cls = (init cls, last cls)

class BoundId w i where
  boundConst :: w -> TermConst -> i

data Indexed w i = Idx w i


refreshClause ::
  ( MonadProp m
  , Identifier i (TermSet i)
  , CopyTermId w i
  , BoundId w i) =>
  w -> ([TermConst], Clause i) -> m (Clause i)
refreshClause _ _ = undefined
{-refreshClause lsid (binds, trms) =
    forM trms $ \t -> do
        refreshVarsTbl (Idx lsid t) [(b,boundConst lsid b) | b <- binds] t
-}
simpleKBNetwork :: (Forkable m, MonadProp m, Identifier i (TermSet i)) => KB i -> i -> m ()
simpleKBNetwork = simpleKBNetwork' (-1)

--TODO, WARNING: empty clauses!
--TODO: Proper indices!
simpleKBNetwork' :: (Forkable m, MonadProp m, Identifier i (TermSet i)) => Int ->  KB i -> i -> m ()
simpleKBNetwork' _ _ _ = return ()
{-simpleKBNetwork' 0 _ _ = return ()
simpleKBNetwork' fuel kb goal = undefined do
    g <- read goal
    unless (g==bot) $
        disjunctFork () goal [do
            (splitClause -> (pres, post)) <- refreshClause () cls
            eq post goal
            forM_ pres (void . recursiveCall . simpleKBNetwork' (fuel-1) kb)
            |cls <- kb]-}
