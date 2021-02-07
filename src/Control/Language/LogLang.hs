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
  deriving (Show, Eq, Ord)

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
  , Direct i
  , PosTermId i
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
  , Direct i
  , PosTermId i
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
  , Direct i
  , PosTermId i
  , CopyTermId i
  , Std w) =>
  Int -> w ->  KB i -> i -> i -> m ()
simpleKBNetwork'' 0 _ _ _ _ = return ()
simpleKBNetwork'' fuel listId kb goal origGoal = watchFixpoint listId $ do
    g <- read goal
    unless (isBot g) $ do

        gt <- fromCellSize 100 goal
        kbt <- getKBSize 100 kb
        traceM $ "\n" ++ (unlines $ show <$> kbt) ++ "proving " ++ show gt++"\n"

        disjunctForkPromoter goal ("disjunctForkPromoter"::String, listId, goal) $ [do
            --sequence_ $ requestTerm <$> snd cls
            --sequence_ $ watchTermRec <$> snd cls
            (splitClause -> Just (pres, post)) <- refreshClause ("copy" :: String, listId, i::Int) cls

            watchTermRec goal
            eq post goal

            let {traceSolution = watchFixpoint (listId, i) $ ((do
              g' <- read origGoal
              unless (isBot g') $ do
                og <- fromCellSize 100 origGoal
                traceM $ "Possible solution on fixpoint: "++{-show (listId, i)++ -}"\n"++show og
                watchFixpoint (listId, i) traceSolution
            ) :: m ())}
            when (null pres) $ watchTermRec origGoal >> requestTerm origGoal >> traceSolution

            forM_ (zip pres [0..]) $ \(p,j) -> do
              simpleKBNetwork'' (fuel-1) ("simpleKBNetwork''"::String,(fuel-1),p,j::Int,listId,i) kb p origGoal --TODO: pack the kb
              propBot p goal
          |(cls,i) <- zip (clauses kb) [0..]] ++ [ do
              --------------------------------
              --The Split Rule
              --------------------------------
              --traceM $ "Splitting with "++(show $ length $ splittable kb)++" splittables"
              forM_ (zip (axioms kb) [0..]) $ \(ax,j) -> do
                scoped (i,j) $ const $ do
                  (splitClause -> Just (pres, post)) <- refreshClause ("copy" :: String, listId, i::Int, j::Int) ax
                  eq post splitPost

                  simpleKBNetwork'' (fuel-1) ("simpleKBNetwork''"::String,(fuel-1),j::Int,listId,i)
                    --TODO! neither the split post nor split pre are properly read as implications in the KB! KB should be more flexible!
                    (kb{
                        splittable = (deleteAt splitIdx $ splittable kb) ++
                                      (([],) <$> return <$> pres)
                      , axioms = (axioms kb) ++ [([],[splitPost])]})
                      goal origGoal
                  promoteTerm goal
                  pure ()
          |(splitClause.snd -> Just (splitPres, splitPost),splitIdx,i) <- zip3 (splittable kb) [0..] [(length $ clauses kb)..], null splitPres] ++
          [do
              --------------------------------
              --Ex Falsum Quodlibet
              --------------------------------
              --just to make sure they are there...
              sequence $ [read $ head cl | (_,cl) <- clauses kb, length cl == 1]
              --TODO: WARNING: super hacky. Also, KB is not watched for bot!
              watchFixpoint ("e.f.q"::String, listId) $ do
                kbreads <- sequence $ [read $ head cl | (_,cl) <- clauses kb, length cl == 1]
                unless (any (isBot) kbreads) $ do
                  void $ write goal bot
                when (any (isBot) kbreads) $ traceM "e.f.q."
          ] ++ [do
              --------------------------------
              --Implication Elimination
              --------------------------------
              --WARNING: implication hard wired!
              let imp = direct (listId, "impltrm"::String)
                  impr = direct (listId, "implrght"::String)
                  impl = direct (listId, "impllft"::String)
              fromVarsAsCells imp [var impl, ["->", var impr]]
              requestTerm goal --TODO
              eq goal imp
              --TODO: find solution to also transfer universal variables!
              --NOTE: premise does not need to be propagated extra, as it is part of the goal
              --TODO: Just putting a simple implication does not work. The clause needs to be lazily extracted!
              {-}
              watchFixpoint (listId, "FP"::String) $ do
                implt <- fromCellSize 100 impl
                imprt <- fromCellSize 100 impr
                impt <- fromCellSize 100 imp
                traceM $ "Splitting impilcation\n"++show implt++" -> "++show imprt++"\noriginal impl: "++show impt
                -}

              simpleKBNetwork'' (fuel-1) ("simpleKBNetwork'' impl elim"::String,(fuel-1),listId) (kb{splittable = ([],[impl]) : splittable kb}) impr origGoal
          ]

{-
How to prove an implication
Proving the implication A -> B means that forall A, there is a B.
One way to prove it is to assume A into the KB and see whether B follows.
Another way is to say that B holds under ALL proofs of A. For that, A needs to unify with all rules from the KB, just as a goal would. The new A, together with its new premises, are now put into the KB.
The third way to prove is by absurdity. If one of the goals in the KB is bot, then anything follows. This rule cannot apply if there are no nots in the KB. The problem is that it might not be known whether a bot could still occur. Therefore, it needs to be limited to bots up to a certain time. If at a fixpoint no bot has occurred yet, no bot will. This, and this is important, can only be deduced iff enough preliminary information was presented. If in the fixpoint it can be seen that neccessary variables have not been instantiated yet, the e.f.g. deduction cannot be excluded yet.

There is a problem with infinite splits. Any recursive datatype can be split an arbitrary number of times, all of which hold a possible proof. This cannot really be prevented. For the course of this research, we should limit to splitting on datatypes that are non recursive (As we cannot do a proof by recursion yet anyway).
-}

getKBSize :: (MonadProp m, Identifier i (TermSet i)) =>
  Int -> KB i -> m [[TermStruc i]]
getKBSize n kb = forM (clauses kb) $ \(_,c) ->
                  forM c (fromCellSize n)

deleteAt :: Int -> [a] -> [a]
deleteAt k lst
  | k < 0 = lst
  | k >= (length lst) = lst
deleteAt 0 [] = []
deleteAt 0 (_ : xs) = xs
deleteAt n (x : xs) = x : (deleteAt (n-1) xs)
deleteAt _ [] = []








--
