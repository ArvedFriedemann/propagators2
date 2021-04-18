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
simpleKBNetwork = simpleKBNetwork' (-1) --WARNING

simpleKBNetwork' :: forall m v scope n. (MonadProp m v scope, Std n, StdPtr v) => Int -> n -> KB (TermSetPtr v) -> TermSetPtr v -> m ()
simpleKBNetwork' 0 _ _ _ = return ()
simpleKBNetwork' fuel ctx kb (TSP goal) = watchFixpoint (SimpleKBNetwork ctx) $ do
  currg <- read goal
  --cgt <- fromCellSize 100 (TSP goal)
  --cgp <- currScopePtr goal
  --traceM $ "currgoal "++show cgp++" is "++show cgt
  traceM $ "curr network call at fuel "++show fuel
  unless (isBot currg) $ do
    disjunctForkPromote ("djf"::String, ctx) goal $ (flip (zipWith ($))) ([0..] :: [Int]) $ --(if fuel == (-1) then drop 1 else take 1) $ --safeHead $  --WARNING!
      [\i -> do
        (fromJust . splitClause -> (pres, (TSP post))) <- refreshClause ("refresh"::String, i, ctx) cls
        --(fromJust . splitClause -> (pres', (TSP post'))) <- refreshClause ("refresh2"::String, i, ctx) cls

        --watchTermRec (TSP goal) --this was not the issue (phew)

        eq post goal
        --recursive call. Wait for the posterior equality before continuing
        watchFixpoint (SimpleKBNetwork ("checkGoal"::String,ctx,i)) $ do
          g' <- read goal
          {-cgt' <- fromCellSize 100 (TSP goal)
          cspg <- currScopePtr goal
          pcgt' <- fromCellSize 100 (TSP post)
          cpost <- fromCellSize 100 (TSP post')
          traceM $ "\nsubgoal "++show cspg++"("++show goal++") is\n                "++show cgt' ++ "\nwith post:      "++show pcgt'++"\nand clean post: "++show cpost++"\n"-}
          unless (isBot g') $ do
            forM_ pres $ \(TSP pre) -> do
              simpleKBNetwork' (fuel - 1) (SimpleKBNetwork (ctx,i)) kb (TSP pre)
              propBot pre goal
      | cls <- kb] ++ [\i -> do
        --let x = var (TSID @v "eql")
        --let y = var (TSID @v "eqr")
        --(TSP eqstruc) <- fromVarsAsCells (TSID @v "eqt") [x,"/=",y]
        eqstruc <- new (TSID @v $ "eqt"++show (ctx,i))
        --write eqstruc bot
        watch' eqstruc ("tmp"::String) $ \v -> do
          traceM $ "eqstruc in "++show ctx++" value: "++show v
        --eq goal eqstruc --TODO,WARNING: This creates a weird nondeterministic error...seems to be caused by the fromVarsAsCells function...error also occurs when using plain new in a scope...
        --eq goal goal
        watchFixpoint ("tmp2"::String) $ do
          traceM $ "\nwriting bot into "++show goal++"\n"
          write goal bot
          watch' goal ("tmp3"::String) (\v -> do
            traceM $ "goal "++show goal++" is "++show v
            )
        --write goal bot
        --write eqstruc bot
        {-}
        watchFixpoint ("tmp"::String) $ do
          eqs <- fromCellSize @m @v 100 (TSP eqstruc)
          gl <- fromCellSize @m @v 100 (TSP goal)
          traceM $ "Eq match term: "++show eqs++"\nwith goal:     "++show gl-}
          {-}
        watchFixpoint (EqScope, 0::Int) $ do
          g' <- read goal
          when (isBot g') $ do
            traceM "equality structure disproven"
          unless (isBot g') $ do
            traceM "equality structure confirmed"
            s <- newScope EqScope
            scoped s $ do
              let a = var (TSID @v "eqv")
              (TSP eqt) <- fromVarsAsCells (TSID @v "eqt2") [a,"/=",a]
              eq goal eqt
              watchFixpoint (EqScope, 1::Int) $ do
                g <- read goal
                if isBot g
                then return ()
                else parScoped $ write goal bot
                -}
      ]

data EqScope = EqScope
  deriving (Show, Eq, Ord)

data TSID v = TSID String
  deriving (Show, Eq, Ord)
instance Identifier (TSID v) (TermSet (TermSetPtr v))

safeHead :: [a] -> [a]
safeHead [] = []
safeHead (x:_) = [x]








---
