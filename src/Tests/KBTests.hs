module Tests.KBTests where

import "this" Tests.TestLogic
import "this" Data.Terms
import "this" Control.Language.LogLang
import "this" Control.Propagator


data Cell = Sv Int | A | B | C | D | G | H | I | J | K | X | Y | Z deriving (Eq, Ord, Show)
instance Identifier Cell ()
instance Identifier (Cell, Cell) ()

kbtest1 :: IO ()
kbtest1 = runTestSEB @(TermId Cell) $ do
  a <- fromVarsAsCells (DIRECT A) ["A"]
  b <- fromVarsAsCells (DIRECT B) ["B"]
  c <- fromVarsAsCells (DIRECT C) ["C"]
  goal <- return (DIRECT $ Sv 0)
  eq goal b
  kb <- pure [([],[a]),([],[a,b]),([],[c,b])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  {-recursiveCall (C,C) $-}
  simpleKBNetwork' 3 K kb goal
  return [goal]

kbtest1' :: IO ()
kbtest1' = runTestSEB @(TermId Cell) $ do
  a <- fromVarsAsCells (DIRECT A) ["A"]
  b <- fromVarsAsCells (DIRECT B) ["B"]
  c <- fromVarsAsCells (DIRECT C) ["C"]
  goal <- return (DIRECT $ Sv 0)
  kb <- pure [([],[a,b]),([],[c,b])]
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  {-recursiveCall (C,C) $-}
  simpleKBNetwork' 1 K kb goal
  return [a,b,c,goal]

kbtest2 :: IO ()
kbtest2 = runTestSEB @(TermId Cell) $ do
  a <- fromVarsAsCells (DIRECT A) ["A", "K"]
  x <- fromVarsAsCells (DIRECT X) ["A", "X"]
  b <- fromVarsAsCells (DIRECT B) ["X", "B"]
  goal <- fromVarsAsCells (DIRECT G) [var (DIRECT $ Sv 1), "B"]
  kb <- pure [([],[a]),(["X"],[x,b])]

  --test <- refreshVarsTbl D [("X", direct C)] b
  --tests <- refreshClause D (["X"], [x,b])
  simpleKBNetwork' 3 K kb goal
  return $ [goal] -- ++ tests
