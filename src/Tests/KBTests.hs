module Tests.KBTests where

import "this" Tests.TestLogic
import "this" Data.Terms.TermFunctions
import "this" Control.Language.LogLang
import "this" Control.Propagator.Class

kbtest1 :: IO ()
kbtest1 = runTestSEB $ do
  a <- fromVarsAsCells (ls [ccon "A"])
  b <- fromVarsAsCells (ls [ccon "B"])
  c <- fromVarsAsCells (ls [ccon "C"])
  kb <- pure [([],[a]),([],[a,b]),([],[c,b])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  recursiveCall $ simpleKBNetwork kb b
  return [a]
