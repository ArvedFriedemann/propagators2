{-# LANGUAGE NoImplicitPrelude #-}
module Control.Combinator.TypeTheoryTools where

import "base" Prelude hiding ( read )
import "base" Control.Monad
import "base" Data.Typeable
import "base" Debug.Trace

import "this" Data.Terms.Terms
import "this" Data.Terms.TermId
import "this" Data.Terms.TermFunctions
import "this" Control.Propagator
import "this" Data.Lattice
import "this" Control.Combinator.Logics

import "containers" Data.Map qualified as Map
import "containers" Data.Set (Set)
import "containers" Data.Set qualified as Set

type Constnts = Set.Set TermConst

data LangDef = LangDef {
  implConst :: TermConst
}

type LazKB i = [(Constnts,i)]

refreshTerm ::
  ( MonadProp m
  , Identifier i (TermSet i)
  , CopyTermId i
  , Bound i
  , Std w) =>
  w -> (Constnts, i) -> m i
refreshTerm _ (Set.null -> True, trm) = pure trm
refreshTerm lsid (binds, trm)
    = refreshVarsTbl lsid (Map.fromSet (bound lsid) binds) trm

lazySearch :: forall m i w.
  (MonadProp m
  , Typeable m
  , Identifier i (TermSet i)
  , Promoter i (TermSet i) m
  , CopyTermId i
  , Bound i
  , Direct i
  , PosTermId i
  , Std w) => w -> LazKB i -> i -> i -> m ()
lazySearch = lazySearch'

lazySearch' :: forall m i w.
  (MonadProp m
  , Typeable m
  , Identifier i (TermSet i)
  , Promoter i (TermSet i) m
  , CopyTermId i
  , Bound i
  , Direct i
  , PosTermId i
  , Std w) => w -> LazKB i -> i -> i -> m ()
lazySearch' listId kb goal origGoal = do
  --specifically get the terms needed TODO: necessary?
  requestTerm goal
  sequence_ $ requestTerm . snd <$> kb
  watchFixpoint listId $ do
    currg <- read goal
    unless (isBot currg) $ do
      gt <- fromCellSize 100 goal
      kbt <- sequence $ fromCellSize 100 . snd <$> kb
      traceM $ "\n" ++ (unlines $ show <$> kbt) ++ "proving:\n" ++ show gt++"\n"

    unless (isBot currg) $ disjunctForkPromoter goal ("djf"::String,listId) $
      flip (zipWith ($)) [0..] $
        [\i -> do
          cpy <- refreshTerm ("refr"::String, listId, i::Int) cls
          eq goal cpy

          let {traceSolution = ((watchFixpoint (listId, i) $ do
            g' <- read origGoal
            unless (isBot g') $ do
              og <- fromCellSize 100 origGoal
              traceM $ "Possible solution on fixpoint: "++{-show (listId, i)++ -}"\n"++show og
              watchFixpoint (listId, i) traceSolution
          ) :: m ())}
          watchTermRec origGoal >> requestTerm origGoal >> traceSolution

        | cls <- kb] ++ [\i -> do
          --this refresh will be called countless times (once for every premise)
          --currently handled by refresh not being called when there are no bound variables
          cpy <- refreshTerm ("refr"::String, listId, i::Int) cls
          --this is dirty, but ok as we are in a safe scope
          (imp, impl, impr) <- matchImpl (listId,i::Int) cpy
          propBot imp goal --technically not needed, but might make things faster
          lazySearch' ("prem"::String,listId,i::Int) kb impl origGoal
          lazySearch' ("post"::String,listId,i::Int) ((Set.empty, impr) : kb) goal origGoal
          --WARNING: this causes super high branching factor. This should maybe commit to a clause, but that will obviously not work when the clause hasn't yet been build
        | cls <- kb] ++ [\i -> do
          (_, impl, impr) <- matchImpl (listId,i::Int) goal
          --no propBot needed here because goal is already matched
          lazySearch' ((),listId,i::Int) ((Set.empty,impl) : kb) impr origGoal
        ] {- ++ [\i -> do
          --In the e.f.q case, more than the goal would need to be promoted. Here, the chosen clause would need to be set to bot in the orig. Problem: There might not be a reason as to why it should be bot.
        | cls <- kb]-}

matchImpl :: (MonadProp m, Identifier i (TermSet i), PosTermId i, Direct i, Std w) => w -> i -> m (i,i,i)
matchImpl listId trm = do
  let imp = direct (listId, "impltrm"::String)
      impr = direct (listId, "implrght"::String)
      impl = direct (listId, "impllft"::String)
  fromVarsAsCells imp [var impl, ["->", var impr]]
  eq trm imp
  return (imp, impl, impr)

{-
split the goal for being an implication. Make the new premises splittable, the old ones stay as is. These new premises change quantifier direction. They need each possible proof.
Any part in a KB that is not an axiom can be "split", meaning it has to hold for all proofs in the axioms that it descends from.
-}

{-
Rules that need to be implemented:

b in b
b in (a -> b)
b in a -> b in (a -> b)

b in a -> (a -> b) --id
(c -> b) in a -> ((c in a -> bot) -> bot) -> (a -> b) --function application
(k in a) -> ((a -> k) -> b) -> (a -> b) --split (kinda needs split to work already though...)
--(bot -> a) is a given rule, not an axiom

A in (A :: AS)
A in XS -> A in (X :: XS)
A in [] -> bot

(b : B) in K -> proof (b : B) from K
(f : (p' : P') => Q') in K ->
  exchange p' with p in (f : (p' : P') => Q') to (f : (p : P) => Q) ->
  proof (p : P) from K ->
  proof (b : B) from ((f p : Q) :: K) ->
  proof (b : B) from K
proof B from (A :: K) ->  proof (b : A => B) from K



--refresh proof variable so it gets refreshed in the types!
--TODO: Where to put e.f.q?


--Notes on Split:
(X -> List X -> P) -> P -> (l : List X) -> P
(a = a) -> (a = b) -> P         --this is the wired in part: a = a and a = b
IMPORTANT: Polymorphic equality as a constructor and eliminator
  data _=_ {A : Type} (x : A) : A -> Type where
    refl : x = x

  (x=x -> C) -> C  --reduction
  (A : Type) -> (x : A) -> (x : A) -> x = x  --introduction

  --Note: multi binding of proofs should maybe not be possible, or be treated differently. Here, binding the x to a fresh variable would bring confusion when trying to rebind it! But, in our setting, the second argument is not needed, as one argument is sufficient to create the equivalence rule.

  The proofs x : ... are just binders. In the original definition, the second A is to make sure the type of the second argument fits. Maybe not necessary here, but hay. One thing that we can do that AGDA cannot is to reuse proof variables. It simply makes sure that the two proofs unify. Problem here will be higher order unification. In that case, the reduction rule does not hold anymore. Just because two things do not unify normally does not mean the do not unify at all. What we want is that we can give extra equivalences that are used in the unification process. in that case, we can have the additional rules of equality, among which:
    a=a' -> b=b' -> a b = a' b'
    x -> x=a -> a
  This would mean that one can freely exchange a type with its equivalency class. In the reduction that means, that there are more ways to create an equality, including transitivity
    a=b -> b=c -> a=c
  These proofs can maybe not be created, but they clog up the memory. Also, the reduction becomes problematic because transitivity always applies and there is no way yet to make it stop. It can, in fact, only stop, when all possible equivalence proofs available have been considered! Problem is, there could be arbitrarily many. The idea is that a proof of equivalence by unification outranks a proof by transitivity. Therefore, the proof may stop in case the equivalence was proven. Inequality is a lot harder to proof though. In the simple case of id, transitivity can be applied infinitely often, which is outranked by the proof by unification.


  Quick Idea: the search process itself might not terminate. A termination checker can find that out and give bot as a result!

--technically, this is the split rule. Only Modus Ponens really needed here
--more general case: This rule is created from the KB! Anything follows, if it follows from everything that is created! Axioms cannot be split...that might need to go in there by hand...or rather, they could be split, but it's propably not leading anywhere. Also, the rule does not need to be lazily recreated, as it is fine when it holds for the axioms. Same rationale as to why the rule itself does not need to be included.
--It is not a rule though as it acts upon the KB. THerefore needs special treatment.
(A v B) -> (A -> C) -> (B -> C) -> C
A -> (A v B)
B -> (A v B)

(A ^ B) -> A
(A ^ B) -> B
A -> B -> (A ^ B)

(A -> B) -> (B -> C) -> A -> C
(A -> A)

--split for equality: (though this does need the explicit split to work)
(x=x -> C) -> C

Therefore: the split needs to be hard wired!
A => B, then A can be split by implications from the KB.
There is two ways how these splits can go. Either, the plit is made with all premises that were and that still follow. This makes sense as premises can be distributive. To save proof capacities, a rule can have explicit constructors, like a data type. These could come in the form of an elimination rule. As the elimination rule can always be deduced by the general split rule, this does not need to be explicitly given. But there is no other way to have knowledge from the KB flow into an implication.
No, wait. the above case.
from
x=x
proof
(x=x -> C) -> C

from
x=x
x=x -> C
proof
C

from
x=x
x=x -> C
proof
x=x

now, only the recursing proof path remains, but that is ok. Now, only the proof search needs to be adjusted to handle implications better.

from
x=x
K -> x=x
proof
(x=x -> C) -> C

from
x=x
K -> x=x
x=x -> C
proof
C

from
x=x
K -> x=x
x=x -> C
proof
C

This still works, but shouldn't. No wait...it totally can work. Because we just assume we already have the implication. but that would work for any rule with just a unary constructor.
Problem is: there is a difference between a mono and polymorphic input. repl is actually a polymorphic function. Technically, what's given should only be the reduction functions.
-}
