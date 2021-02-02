module Control.Combinator.TypeTheoryTools where


import Data.Terms.Terms
import Data.Terms.TermFunctions


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








-}
