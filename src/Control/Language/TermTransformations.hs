module Control.Language.TermTransformations where

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Data.Terms.TermId
import "this" Control.Language.LogLang hiding (clauses)
import "this" Control.Propagator
import "this" Data.Some

import "this" Control.Combinator.TypeTheoryTools

import "containers" Data.Map qualified as Map
import "containers" Data.Set qualified as Set

import "base" Debug.Trace

buildBoundTerm :: (Std a, Std k) =>
  k -> TermStruc a -> (Consts, TermStruc TermId)
buildBoundTerm k ts = (Set.fromList varConsts
                        , exchangedVars)
  where varTrans = GEN . Some . (k,)
        varConsts = varTrans <$> tsVars ts
        exchangedVars = (exchangeVars $  SCON . varTrans) ts

buildBoundTermM :: (MonadProp m, Std a, Std k) =>
  k -> TermStruc a -> m (Consts, TermId)
buildBoundTermM k ts = do
  let (bounds, clause) = buildBoundTerm k ts
  clauseM <- fromVarsAsCells (DIRECT k) clause
  return (bounds, clauseM)

buildLazyKBM :: (MonadProp m, Std a, Std k) =>
  k -> [TermStruc a] -> m (LazKB TermId)
buildLazyKBM k ts = sequence [buildBoundTermM (k,i::Int) t | (t,i) <- zip ts [0..]]

setupLazySearch :: (MonadProp m, Std a, Std k) =>
  k -> [TermStruc a] -> TermStruc a -> m (LazKB TermId, TermId)
setupLazySearch k clauses goal = do
  let goalStruc = fmap (DIRECT . (k,)) goal
  goal' <- fromVarsAsCells (DIRECT $ (k,"goal"::String)) goalStruc
  kb <- buildLazyKBM (k,"KB"::String) clauses
  return (kb, goal')








buildClause :: (Std a, Std k) =>
  k -> TermStruc a -> TermStruc a -> (Consts, Clause (TermStruc TermId))
buildClause k implOp ts = (Set.fromList varConsts
                        , exchangedVars)
  where varTrans = GEN . Some . (k,)
        varConsts = varTrans <$> tsVars ts
        implparts = rassocOp implOp ts
        exchangedVars = (exchangeVars $  SCON . varTrans) <$> implparts

buildClauseM :: (MonadProp m, Std a, Std k) =>
  k -> TermStruc a -> TermStruc a -> m (Consts, Clause TermId)
buildClauseM k implOp ts = do
  let (bounds, clause) = buildClause k implOp ts
  clauseM <- sequence [fromVarsAsCells (DIRECT (k,i::Int)) t | (t,i) <- zip clause [0..]]
  return (bounds, clauseM)

buildKBM :: (MonadProp m, Std a, Std k) =>
  k -> TermStruc a -> [TermStruc a] -> m (KB TermId)
buildKBM k implOp clauses = do
  axs <- sequence $ [buildClauseM (k,i::Int) implOp c | (c,i) <- zip clauses [0..]]
  return $ KB {axioms = axs, splittable = []}

setupSearch :: (MonadProp m, Std a, Std k) =>
  k -> TermStruc a -> [TermStruc a] -> TermStruc a-> m (KB TermId, TermId)
setupSearch k implOp clauses goal = do
  let goalStruc = fmap (DIRECT . (k,)) goal
  goal' <- fromVarsAsCells (DIRECT $ (k,"goal"::String)) goalStruc
  kb <- buildKBM (k,"KB"::String) implOp clauses
  return (kb, goal')




--
