{-# LANGUAGE NoImplicitPrelude #-}
module Control.Language.LogLang where

import "base" Prelude hiding ( read )
import "base" GHC.Exts
import "base" Data.Tuple
import "base" Data.Bifunctor
import "base" Control.Monad

import "containers" Data.Map qualified as Map
import "containers" Data.Set qualified as Set

import "this" Data.Terms
import "this" Control.Combinator.Logics
import "this" Control.Propagator
import "this" Data.Lattice


type Clause i = Facts i

newtype Consts = Consts (Facts TermConst)
  deriving newtype
    ( Eq, Ord
    , HasTop, HasBot
    , Join,    BoundedJoin
    , Meet,    BoundedMeet
    , Lattice, BoundedLattice
    )
instance Show Consts where
    showsPrec d (Consts Bot)
        = showParen (d >= 10)
        $ showString "Consts Bot"
    showsPrec d (Consts (Facts s))
        = showParen (d >= 10)
        $ showString "Consts "
        . shows (Set.toList s)

--clauses need to memorise their universal variables
type KB i = [(Facts TermConst, Clause i)]

splitClause :: Clause i -> Maybe (Clause i, i)
splitClause cl = (first Facts . swap) <$> (Set.minView $ getFacts cl)


refreshClause ::
  ( MonadProp m
  , Identifier i (TermSet i)
  , CopyTermId w i
  , Bound w i
  , Std w) =>
  w -> (Facts TermConst, Clause i) -> m (Clause i)
refreshClause lsid (binds, trms)
    = fmap fromList
    . forM (toList trms) 
    $ refreshVarsTbl lsid
    . Map.fromSet (bound lsid) 
    . getFacts
    $ binds

data SimpleKBNetwork w i = SBNC w i
  deriving (Eq, Ord, Show)
instance (Std w, Std i) => Identifier (SimpleKBNetwork w i) ()

data Lower w i = LW w i | LWDirect w
  deriving (Eq, Ord, Show)

simpleKBNetwork ::
  ( MonadProp m
  , MonadFail m
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
