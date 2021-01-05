module Tests.KBTests where

import "this" Tests.TestLogic
import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Data.Terms.TermId
import "this" Control.Language.LogLang
import "this" Control.Propagator.Class
import "this" Control.Propagator.Combinators
import "this" Data.Facts

data Cell = Sv Int | A | B | C | G | H | I | J | X | Y | Z deriving (Eq, Ord, Show)
instance Identifier Cell UnitFact
instance Identifier (Cell, Cell) UnitFact

kbtest1 :: IO ()
kbtest1 = runTestSEB @(TermId Cell) $ do
  a <- fromVarsAsCells (direct A) ["A"]
  b <- fromVarsAsCells (direct B) ["B"]
  c <- fromVarsAsCells (direct C) ["C"]
  goal <- return (direct $ Sv 0)
  eq goal b
  kb <- pure [([],[a]),([],[a,b]),([],[c,b])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  recursiveCall (C,C) $ simpleKBNetwork' 3 kb goal
  return [goal]


kbtest2 :: IO ()
kbtest2 = runTestSEB @(TermId Cell) $ do
  a <- fromVarsAsCells (direct A) ["A", "K"]
  x <- fromVarsAsCells (direct X) ["A", "X"]
  b <- fromVarsAsCells (direct B) ["X", "B"]
  goal <- fromVarsAsCells (direct G) [var (direct $ Sv 1), "B"]
  eq goal b
  kb <- pure [([],[a]),(["X"],[x,b])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  recursiveCall (C,C) $ simpleKBNetwork' 2 kb goal
  return [goal]
