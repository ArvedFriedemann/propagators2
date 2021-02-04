module Control.Combinator.TypeTheoryTools where


import Data.Terms.Terms
import Data.Terms.TermFunctions


data LangDef = LangDef {
  implConst :: TermConst
}



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

-}
