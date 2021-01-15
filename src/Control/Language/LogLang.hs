{-# LANGUAGE NoImplicitPrelude #-}
module Control.Language.LogLang where

import "base" Prelude hiding ( read )
import "base" GHC.Exts
import "base" Control.Monad
import "base" Data.Typeable

import "containers" Data.Map qualified as Map
import "containers" Data.Set qualified as Set

import "this" Data.Terms
import "this" Control.Combinator.Logics
import "this" Control.Propagator
import "this" Data.Lattice


type Clause = []

type Consts = Set.Set TermConst

--clauses need to memorise their universal variables
type KB i = [(Consts, Clause i)]

splitClause :: Clause i -> Maybe (Clause i, i)
splitClause cl
    | not $ null cl = Just (init cl, last cl)
    | otherwise     = Nothing

refreshClause ::
  ( MonadProp m
  , Identifier i (TermSet i)
  , CopyTermId w i
  , Bound w i
  , Std w) =>
  w -> (Consts, Clause i) -> m (Clause i)
refreshClause lsid (binds, trms)
    = forM (toList trms) $ refreshVarsTbl lsid . Map.fromSet (bound lsid) $ binds

data SimpleKBNetwork w i = SBNC w i
  deriving (Eq, Ord, Show)
instance (Std w, Std i) => Identifier (SimpleKBNetwork w i) ()

data Lower w i = LW w i | LWDirect w
  deriving (Eq, Ord, Show)

simpleKBNetwork ::
  ( MonadProp m
  , MonadFail m
  , Typeable m
  , Identifier i (TermSet i)
  , Bound w i
  , CopyTermId w i
  , Identifier w a
  , BoundedJoin a
  , BoundedJoin a
  , Std w) =>
  w -> KB i -> i -> m ()
simpleKBNetwork = simpleKBNetwork' (-1)

--TODO, WARNING: empty clauses!
--TODO: Proper indices!
simpleKBNetwork' ::
  ( MonadProp m
  , MonadFail m
  , Typeable m
  , Identifier i (TermSet i)
  , Bound w i
  , CopyTermId w i
  , Identifier w a
  , BoundedJoin a
  , Std w) =>
  Int -> w ->  KB i -> i -> m ()
simpleKBNetwork' 0 _ _ _ = return ()
simpleKBNetwork' fuel listId kb goal = do
    g <- read goal
    unless (g==bot) $
        disjunctFork listId goal [do
            (splitClause -> Just (pres, post)) <- refreshClause listId cls
            eq post goal
            --TODO: recursive Call on listId probably wrong
            forM_ pres (void . recursiveCall listId . simpleKBNetwork' (fuel-1) listId kb)
            |cls <- kb]
