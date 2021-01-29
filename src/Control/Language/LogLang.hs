{-# LANGUAGE NoImplicitPrelude #-}
module Control.Language.LogLang where

import "base" Prelude hiding ( read )
import "base" GHC.Exts
import "base" Control.Monad
import "base" Data.Typeable
import "base" Debug.Trace

import "containers" Data.Map qualified as Map
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

--TODO, WARNING: empty clauses!
--TODO: Proper indices!
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
simpleKBNetwork' 0 _ _ _ = return ()
simpleKBNetwork' fuel listId kb goal = do
    g <- read goal
    unless (g==bot) $ do
        disjunctForkPromoter goal ("disjunctForkPromoter"::String, listId, goal) [do
            --sequence_ $ requestTerm <$> snd cls
            --sequence_ $ watchTermRec <$> snd cls
            (splitClause -> Just (pres, post)) <- refreshClause ("copy" :: String, listId, i::Int) cls --TODO: might have to be indexed with cls as well
            watchTermRec goal
            --watchTermRec post
            eq post goal
            forM_ (zip pres [0..]) $ \(p,j) -> do
              simpleKBNetwork' (fuel-1) ("simpleKBNetwork'"::String,(fuel-1),p,j::Int,listId,i) kb p --TODO: pack the kb
              propBot p goal
              --watchTermRec p
            |(cls,i) <- zip kb [0..]]











--
