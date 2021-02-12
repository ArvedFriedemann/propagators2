module Control.Language.TermTransformations where

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Data.Terms.TermId
import "this" Control.Language.LogLang
import "this" Control.Propagator
import "this" Data.Some

import "unordered-containers" Data.HashSet qualified as Set


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
buildKBM k implOp clauses = sequence $ [buildClauseM (k,i::Int) implOp c | (c,i) <- zip clauses [0..]]

setupSearch :: (MonadProp m, Std a, Std k) =>
  k -> TermStruc a -> [TermStruc a] -> TermStruc a-> m (KB TermId, TermId)
setupSearch k implOp clauses goal = do
  let goalStruc = fmap (DIRECT . (k,)) goal
  goal' <- fromVarsAsCells (DIRECT $ (k,"goal"::String)) goalStruc
  kb <- buildKBM (k,"KB"::String) implOp clauses
  return (kb, goal')




--
