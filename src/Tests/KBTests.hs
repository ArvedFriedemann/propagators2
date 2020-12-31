module Tests.KBTests where

import "this" Tests.TestLogic
import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Language.LogLang
import "this" Control.Propagator.Class

kbtest1 :: IO ()
kbtest1 = runTestSEB $ do
  a <- fromVarsAsCells (ls [ccon "A"])
  b <- fromVarsAsCells (ls [ccon "B"])
  c <- fromVarsAsCells (ls [ccon "C"])
  goal <- fromVarsAsCells (ls [])
  eq goal b
  kb <- pure [([],[a]),([],[a,b]),([],[c,b])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  recursiveCall $ simpleKBNetwork' 3 kb goal
  return [goal]


kbtest2 :: IO ()
kbtest2 = runTestSEB $ do
  gv <- fromVarsAsCells (ls [])
  a <- fromVarsAsCells (ls [ccon "A", ccon "K"])
  x <- fromVarsAsCells (ls [ccon "A", ccon "X"])
  b <- fromVarsAsCells (ls [ccon "X", ccon "B"])
  goal <- fromVarsAsCells (ls [var gv, ccon "B"])
  eq goal b
  kb <- pure [([],[a]),([scon "X"],[x,b])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  recursiveCall $ simpleKBNetwork' 2 kb goal
  return [goal]
