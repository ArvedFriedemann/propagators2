module Tests.KBTests where

import "this" Tests.TestLogic
import "this" Data.Terms
import "this" Control.Language.LogLang
import "this" Control.Propagator


data Cell = Sv Int | A | B | C | D | G | H | I | J | K | X | Y | Z deriving (Eq, Ord, Show)
instance Identifier Cell ()
instance Identifier (Cell, Cell) ()

kbtest0 :: IO ()
kbtest0 = runTestSEB @(TermId) $ do
  a <- fromVarsAsCells (DIRECT A) ["A"]
  --var (DIRECT $ Sv 1)
  --goal <- fromVarsAsCells (DIRECT $ Sv 0) ["A", "C"]
  goal <- fromVarsAsCells (DIRECT $ Sv 0) ["B"]
  kb <- pure [(["A"],[a])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  {-recursiveCall (C,C) $-}
  simpleKBNetwork' 3 K kb goal
  return [goal]

kbtest0' :: IO ()
kbtest0' = runTestSEB @(TermId) $ do
  a <- fromVarsAsCells (DIRECT A) ["A"]
  goal <- fromVarsAsCells (DIRECT $ Sv 0) []
  kb <- pure [([],[a,a])]
  --this gives a solution because even though the proof failed, it is certain that the a needs to be unified.
  simpleKBNetwork' 1 K kb goal
  return [goal]

kbtest1 :: IO ()
kbtest1 = runTestSEB @(TermId) $ do
  a <- fromVarsAsCells (DIRECT A) ["A"]
  b <- fromVarsAsCells (DIRECT B) ["B"]
  c <- fromVarsAsCells (DIRECT C) ["C"]
  goal <- fromVarsAsCells (DIRECT $ Sv 0) []
  eq goal b
  kb <- pure [([],[a]),([],[a,b]),([],[c,b])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  {-recursiveCall (C,C) $-}
  simpleKBNetwork' 3 K kb goal
  return [goal]

kbtest1' :: IO ()
kbtest1' = runTestSEB @(TermId) $ do
  a <- fromVarsAsCells (DIRECT A) ["A"]
  b <- fromVarsAsCells (DIRECT B) ["B"]
  --c <- fromVarsAsCells (DIRECT C) ["C"]
  goal <- fromVarsAsCells (DIRECT $ Sv 0) ["B"]
  kb <- pure [([],[a,b]){-,([],[a])-}]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  {-recursiveCall (C,C) $-}
  simpleKBNetwork' 2 K kb goal
  return [a,b,{-c,-}goal]

kbtest2 :: IO ()
kbtest2 = runTestSEB @(TermId) $ do
  a <- fromVarsAsCells (DIRECT A) ["A", "B"]
  x <- fromVarsAsCells (DIRECT X) ["A", "X"]
  b <- fromVarsAsCells (DIRECT B) ["X", "B"]
  goal <- fromVarsAsCells (DIRECT G) [var (DIRECT $ Sv 1), "B"]
  kb <- pure [([],[a]),(["X"],[x,b])]

  --test <- refreshVarsTbl D [("X", direct C)] b
  --tests <- refreshClause D (["X"], [x,b])
  simpleKBNetwork' 2 K kb goal
  return $ [goal] -- ++ tests
