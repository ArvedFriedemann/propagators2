{-# LANGUAGE NoImplicitPrelude #-}

module Control.Language.LogLang where

import "base" Prelude hiding ( read )
import "base" Control.Monad

import "this" Data.Terms.Terms
import "this" Data.Terms.TermId
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

data RefreshClause w i = RC w i
  deriving (Eq, Ord, Show)

refreshClause ::
  ( MonadProp m
  , Identifier i (TermSet i)
  , CopyTermId w i
  , Bound w i
  , Std w) =>
  w -> ([TermConst], Clause i) -> m (Clause i)
refreshClause lsid (binds, trms) =
    forM trms $ \t -> do
        refreshVarsTbl lsid [(b, bnd lsid b) | b <- binds] t

data SimpleKBNetwork w i = SBNC w i
  deriving (Eq, Ord, Show)
instance (Std w, Std i) => Identifier (SimpleKBNetwork w i) ()

data Lower w i = LW w i | LWDirect w
  deriving (Eq, Ord, Show)

simpleKBNetwork ::
  ( Forkable m
  , MonadProp m
  , Identifier i (TermSet i)
  , Bound w i
  , CopyTermId w i
  , Identifier w a
  , Std w) =>
  w -> KB i -> i -> m ()
simpleKBNetwork = simpleKBNetwork' (-1)

--TODO, WARNING: empty clauses!
--TODO: Proper indices!
simpleKBNetwork' ::
  ( Forkable m
  , MonadProp m
  , Identifier i (TermSet i)
  , Bound w i
  , CopyTermId w i
  , Identifier w a
  , Std w) =>
  Int -> w ->  KB i -> i -> m ()
simpleKBNetwork' 0 _ _ _ = return ()
simpleKBNetwork' fuel listId kb goal = do
    g <- read goal
    unless (g==bot) $
        disjunctFork listId goal [do
            (splitClause -> (pres, post)) <- refreshClause listId cls
            eq post goal
            --TODO: recursive Call on listId probably wrong
            forM_ pres (void . recursiveCall listId . simpleKBNetwork' (fuel-1) listId kb)
            |cls <- kb]
