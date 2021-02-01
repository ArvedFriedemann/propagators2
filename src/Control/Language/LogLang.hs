{-# LANGUAGE NoImplicitPrelude #-}
module Control.Language.LogLang where

import "base" Prelude hiding ( read )
import "base" GHC.Exts
import "base" Control.Monad
import "base" Data.Typeable
import "base" Debug.Trace

import "containers" Data.Map qualified as Map
import "containers" Data.Set (Set)
import "containers" Data.Set qualified as Set

import "this" Data.Terms
import "this" Control.Combinator.Logics
import "this" Control.Propagator
import "this" Control.Propagator.Propagator
import "this" Control.Propagator.Combinators
import "this" Data.Lattice


type Clause = []

type Consts = Set.Set TermConst

--clauses need to memorise their universal variables
data KB i = KB {
  axioms :: [(Consts, Clause i)],
  splittable :: [(Consts, Clause i)]
}

clauses :: KB i -> [(Consts, Clause i)]
clauses kb = (axioms kb) ++ (splittable kb)

splitClause :: Clause i -> Maybe (Clause i, i)
splitClause [] = Nothing
splitClause cl = Just (init cl, last cl)

refreshClause ::
  ( MonadProp m
  , Identifier i (TermSet i)
  , CopyTermId i
  , Bound i
  , Std w) =>
  w -> (Consts, Clause i) -> m (Clause i)
refreshClause lsid (binds, trms)
    = forM (zip trms [0..]) $ \(t,i) ->
        refreshVarsTbl (lsid,i::Int) (Map.fromSet (bound lsid) binds) t

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
  , Promoter i (TermSet i) m
  , Bound i
  , CopyTermId i
  , Std w) =>
  w -> KB i -> i -> m ()
simpleKBNetwork = simpleKBNetwork' (-1)


simpleKBNetwork' :: forall m i w .
  ( MonadProp m
  , MonadFail m
  , Typeable m
  , Identifier i (TermSet i)
  , Promoter i (TermSet i) m
  , Bound i
  , CopyTermId i
  , Std w) =>
  Int -> w ->  KB i -> i -> m ()
simpleKBNetwork' fuel listId kb goal = simpleKBNetwork'' fuel listId kb goal goal

--TODO, WARNING: empty clauses!
--TODO: Proper indices!
simpleKBNetwork'' :: forall m i w .
  ( MonadProp m
  , MonadFail m
  , Typeable m
  , Identifier i (TermSet i)
  , Promoter i (TermSet i) m
  , Bound i
  , CopyTermId i
  , Std w) =>
  Int -> w ->  KB i -> i -> i -> m ()
simpleKBNetwork'' 0 _ _ _ _ = return ()
simpleKBNetwork'' fuel listId kb goal origGoal = watchFixpoint listId $ do
    g <- read goal
    unless (g==bot) $ do
        --traceM $ "Executing branch "++show listId
        disjunctForkPromoter goal ("disjunctForkPromoter"::String, listId, goal) $ [do
            --sequence_ $ requestTerm <$> snd cls
            --sequence_ $ watchTermRec <$> snd cls
            (splitClause -> Just (pres, post)) <- refreshClause ("copy" :: String, listId, i::Int) cls

            watchTermRec goal
            eq post goal

            let {traceSolution = watchFixpoint (listId, i) $ ((do
              g' <- read origGoal
              unless (g' == bot) $ do
                og <- fromCellSize 100 origGoal
                traceM $ "Possible solution on fixpoint: "++{-show (listId, i)++ -}"\n"++show og
                watchFixpoint (listId, i) traceSolution
            ) :: m ())}
            when (null pres) $ watchTermRec origGoal >> requestTerm origGoal >> traceSolution

            forM_ (zip pres [0..]) $ \(p,j) -> do
              simpleKBNetwork'' (fuel-1) ("simpleKBNetwork''"::String,(fuel-1),p,j::Int,listId,i) kb p origGoal --TODO: pack the kb
              propBot p goal
          |(cls,i) <- zip (clauses kb) [0..]] ++ [ do
              forM_ (zip (axioms kb) [0..]) $ \(ax,j) -> do
                scoped (i,j) $ const $ do
                  (splitClause -> Just (pres, post)) <- refreshClause ("copy" :: String, listId, i::Int, j::Int) ax
                  eq post splitPost
                  simpleKBNetwork'' (fuel-1) ("simpleKBNetwork''"::String,(fuel-1),j::Int,listId,i) (kb{splittable = (deleteAt splitIdx $ splittable kb)++ (([],) <$> return <$> pres) ++ [([],[post])]}) goal origGoal
                  promote goal
                  --TODO: This needs explicit ex falsum quodlibet rule!
                  pure ()
          |(splitClause.snd -> Just (splitPres, splitPost),splitIdx,i) <- zip3 (splittable kb) [0..] [(length $ clauses kb)..], null splitPres]



deleteAt :: Int -> [a] -> [a]
deleteAt k lst
  | k < 0 = lst
  | k >= (length lst) = lst
deleteAt 0 [] = []
deleteAt 0 (_ : xs) = xs
deleteAt n (x : xs) = x : (deleteAt (n-1) xs)
deleteAt _ [] = []








--
