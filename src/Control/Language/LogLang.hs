{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}
module Control.Language.LogLang where

import "base" Prelude hiding ( read )
import "base" Control.Monad
import "base" Control.Arrow
import "base" Data.Typeable
--import "base" Debug.Trace
import "base" GHC.Generics

import "unordered-containers" Data.HashMap.Strict qualified as Map
import "unordered-containers" Data.HashSet qualified as Set

import "hashable" Data.Hashable

import "this" Data.Terms
import "this" Control.Combinator.Logics
import "this" Control.Propagator
import "this" Data.Lattice


type Clause = []

type Consts = Set.HashSet TermConst

--clauses need to memorise their universal variables
type KB i = [(Consts, Clause i)]

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
        refreshVarsTbl (lsid,i::Int) (Map.fromList $ (id &&& bound lsid) <$> Set.toList binds ) t

data SimpleKBNetwork w i = SBNC w i
  deriving (Eq, Ord, Show, Generic)
instance (Hashable w, Hashable i) => Hashable (SimpleKBNetwork w i)
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
{-# INLINE simpleKBNetwork #-}


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
{-# INLINE simpleKBNetwork' #-}

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
    unless (isBot g) $ do
        --traceM $ "Executing branch "++show listId
        disjunctForkPromoter goal ("disjunctForkPromoter"::String, listId, goal) [do
            --sequence_ $ requestTerm <$> snd cls
            --sequence_ $ watchTermRec <$> snd cls
            (splitClause -> Just (pres, post)) <- refreshClause ("copy" :: String, listId, i::Int) cls

            watchTermRec goal
            eq post goal

            let {traceSolution = watchFixpoint (listId, i) $ ((do
              g' <- read origGoal
              unless (g' == bot) $ do
                --og <- fromCellSize 100 origGoal
                --traceM $ "Possible solution on fixpoint: "++{-show (listId, i)++ -}"\n"++show og
                watchFixpoint (listId, i) traceSolution
            ) :: m ())}
            when (null pres) $ watchTermRec origGoal >> requestTerm origGoal >> traceSolution

            forM_ (zip pres [0..]) $ \(p,j) -> do
              simpleKBNetwork'' (fuel-1) ("simpleKBNetwork''"::String,(fuel-1),p,j::Int,listId,i) kb p origGoal --TODO: pack the kb
              propBot p goal
            |(cls,i) <- zip kb [0..]]











--
