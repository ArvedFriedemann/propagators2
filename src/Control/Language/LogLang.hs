{-# LANGUAGE NoImplicitPrelude #-}
module Control.Language.LogLang where

import "base" Prelude hiding ( read )
import "base" Control.Monad

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

import "this" Data.Terms.Terms
import "this" Control.Propagator.Class

type Clause = []

type Consts = Set TermConst

--clauses need to memorise their universal variables
type KB i = [(Consts, Clause i)]

splitClause :: Clause i -> Maybe (Clause i, i)
splitClause [] = Nothing
splitClause cl = Just (init cl, last cl)

data RefreshConst v ctx c = RefreshConst ctx c
  deriving (Show, Eq, Ord)
instance Identifier (RefreshConst v ctx c) (TermSet (TermSetPtr v))
data RefreshTable ctx i = RefreshTable ctx i
  deriving (Show, Eq, Ord)

refreshClause :: forall m v scope n.
  ( MonadProp m v scope
  , Std n, StdPtr v) =>
  n -> (Consts, Clause (TermSetPtr v)) -> m (Clause (TermSetPtr v))
refreshClause ctx (Set.toList -> binds, trms) = do
  bindVars <- forM binds $ \c -> do
    nc <- new (RefreshConst @v ctx c)
    return (c, TSP nc)
  forM (zip trms ([0..] :: [Int])) $ \(t,i) ->
        refreshVarsTbl (RefreshTable ctx i) (Map.fromList bindVars) t













---
