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
kbtest1 = runTestSEB $ do
  a <- fromVarsAsCells (Direct A) ["A"]
  b <- fromVarsAsCells (Direct B) ["B"]
  c <- fromVarsAsCells (Direct C) ["C"]
  goal <- return (Direct $ Sv 0)
  eq goal b
  kb <- pure [([],[a]),([],[a,b]),([],[c,b])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  recursiveCall (C,C) $ simpleKBNetwork' 3 kb goal
  return [goal]


kbtest2 :: IO ()
kbtest2 = runTestSEB $ do
  a <- fromVarsAsCells (Direct A) ["A", "K"]
  x <- fromVarsAsCells (Direct X) ["A", "X"]
  b <- fromVarsAsCells (Direct B) ["X", "B"]
  goal <- fromVarsAsCells (Direct G) [var (Direct $ Sv 1), "B"]
  eq goal b
  kb <- pure [([],[a]),(["X"],[x,b])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  recursiveCall (C,C) $ simpleKBNetwork' 2 kb goal
  return [goal]
