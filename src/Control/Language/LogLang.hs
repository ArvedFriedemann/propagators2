{-# LANGUAGE NoImplicitPrelude #-}
module Control.Language.LogLang where

import "base" Prelude hiding ( read )
import "base" Control.Monad
import "base" Data.Maybe
import "base" Debug.Trace

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Propagator.Class
import "this" Control.Combinator
import "this" Data.Lattice

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


data SimpleKBNetwork i = SimpleKBNetwork i
  deriving (Show, Eq, Ord)

simpleKBNetwork :: (MonadProp m v scope, Std n, StdPtr v) => n -> KB (TermSetPtr v) -> TermSetPtr v -> m ()
simpleKBNetwork = simpleKBNetwork' (1) --WARNING

simpleKBNetwork' :: (MonadProp m v scope, Std n, StdPtr v) => Int -> n -> KB (TermSetPtr v) -> TermSetPtr v -> m ()
simpleKBNetwork' 0 _ _ _ = return ()
simpleKBNetwork' fuel ctx kb (TSP goal) = watchFixpoint (SimpleKBNetwork ctx) $ do
  currg <- read goal
  cgt <- fromCellSize 100 (TSP goal)
  traceM $ "currgoal "++show goal++" is "++show cgt
  unless (isBot currg) $ do
    disjunctForkPromote ("djf"::String, ctx) goal $ (flip (zipWith ($))) ([0..] :: [Int]) $
      [\i -> do
        (fromJust . splitClause -> (pres, (TSP post))) <- refreshClause ("refresh"::String, i, ctx) cls
        eq post goal
        --recursive call. Wait for the posterior equality before continuing
        watchFixpoint (SimpleKBNetwork ("checkGoal"::String,ctx,i)) $ do
          g' <- read goal
          cgt' <- fromCellSize 100 (TSP goal)
          cspg <- currScopePtr goal
          traceM $ "subgoal "++show cspg++" is "++show cgt'
          unless (isBot g') $ do
            forM_ pres $ \(TSP pre) -> do
              simpleKBNetwork' (fuel - 1) (SimpleKBNetwork (ctx,i)) kb (TSP pre)
              propBot pre goal
      | cls <- kb]











---
