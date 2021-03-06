module Control.Language.TermTransformations where

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Language.LogLang
import "this" Control.Propagator.Class
import "this" Data.Some

import "containers" Data.Map qualified as Map
import "containers" Data.Set qualified as Set

import "base" Debug.Trace

buildClause :: forall v a n. (Std a, Std n) =>
  n -> TermStruc a -> TermStruc a -> (Consts, Clause (TermStruc (GenId v)))
buildClause ctx implOp ts = (Set.fromList varConsts
                        , exchangedVars)
  where varTrans = GEN . Some . (ctx,)
        varConsts = varTrans <$> tsVars ts
        implparts = rassocOp implOp ts
        exchangedVars = (exchangeVars $  SCON . varTrans) <$> implparts

data BuildClauseM v ctx = BuildClauseM ctx
  deriving (Show, Eq, Ord)
instance Identifier (BuildClauseM v ctx) (TermSet (TermSetPtr v))

data GenId v = GenId
  deriving (Show, Eq, Ord)
instance Identifier (GenId v) (TermSet (TermSetPtr v))


buildClauseM :: forall m v scope a n.
  ( MonadProp m v scope
  , Std a, Std n, StdPtr v) =>
  n -> TermStruc a -> TermStruc a -> m (Consts, Clause (TermSetPtr v))
buildClauseM ctx implOp ts = do
  let (bounds, clause) = buildClause @v ctx implOp ts
  clauseM <- sequence [fromVarsAsCells (BuildClauseM @v (ctx,i::Int)) t | (t,i) <- zip clause [0..]]
  return (bounds, clauseM)

buildKBM ::
  ( MonadProp m v scope
  , Std a, Std n, StdPtr v) =>
  n -> TermStruc a -> [TermStruc a] -> m (KB (TermSetPtr v))
buildKBM ctx implOp clauses = sequence $ [buildClauseM (ctx,i::Int) implOp c | (c,i) <- zip clauses [0..]]

data SetupSearch v ctx = SetupSearch ctx
  deriving (Show, Eq, Ord)
instance Identifier (SetupSearch v ctx) (TermSet (TermSetPtr v))

data GenV v ctx c = GenV ctx c
  deriving (Show, Eq, Ord)
instance Identifier (GenV v ctx c) (TermSet (TermSetPtr v))

setupSearch :: forall m v scope n a.
  ( MonadProp m v scope
  , Std a, Std n, StdPtr v) =>
  n -> TermStruc a -> [TermStruc a] -> TermStruc a -> m (KB (TermSetPtr v), (TermSetPtr v))
setupSearch ctx implOp clauses goal = do
  goal' <- fromVarsAsCells (SetupSearch @v (ctx,"goal"::String)) (exchangeVars (SVAR . GenV @v ctx) goal)
  kb <- buildKBM (SetupSearch @v (ctx,"KB"::String)) implOp clauses
  return (kb, goal')




--
