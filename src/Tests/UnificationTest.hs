{-# LANGUAGE NoImplicitPrelude #-}
module Tests.UnificationTest where

import "base" Prelude hiding ( read )
import "base" GHC.Generics
import "base" Control.Monad
import "base" Debug.Trace

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Data.Terms.TermId
import "this" Data.Domain
import "this" Control.Combinator.Logics
import "this" Control.Propagator
import "this" Control.Propagator.Event
import "this" Tests.TestLogic
import "this" Data.Lattice



data Cell = STR String | Sv Int | A | B | C deriving (Eq, Ord, Show)
instance Identifier Cell (TermSet Cell)

test1 :: IO ()
test1 = runTestSEB @(TermId Cell) $ do
    sv_a <- fromVarsAsCells (direct A) $ [var (direct $ Sv 0),"a"]
    b_sv <- fromVarsAsCells (direct B) $ ["b", var (direct $ Sv 0)]
    sv_a `eq` b_sv
    return [sv_a, b_sv, direct $ Sv 0]

test2 :: IO ()
test2 = runTestSEB @(TermId Cell) $ do
    t1 <- fromVarsAsCells (direct A) [var $ direct $ Sv 1, "a", var $ direct $ Sv 1]
    t2 <- fromVarsAsCells (direct B) $ [var (direct $ Sv 2), "a"]
    t1 `eq` t2
    return [t1, t2, direct $ Sv 1, direct $ Sv 2]


test3 :: IO ()
test3 = runTestSEB @(TermId Cell) $ do
    t1 <- fromVarsAsCells (direct A) $ var $direct $ Sv 0
    t2 <- fromVarsAsCells (direct B) $ "a" <> var (direct $ Sv 0)
    t1 `eq` t2
    return [t1, t2, direct $ Sv 0]

test4 :: IO ()
test4 = runTestSEB @(TermId Cell) $ do
  fork () $ \lft -> do
    promote lft (DIRECT A)
    write (DIRECT A) $ constTerm "a"
  return [DIRECT A]

test4' :: IO ()
test4' = runTestSEB @(TermId Cell) $ do
    orig <- return (direct A :: TermId Cell)
    t1 <- return (direct B)
    t2 <- return (direct C)
    write t1 (constTerm (CUST "A"))
    write t2 TSBot
    --TODO: When forking, it might not be the entire term that is being transferred, but only the top node!
    disjunctFork () orig
        [ do
            --void $ write orig (constTerm "A")
            orig `eq` t1
        , do
            --void $ write orig TSBot
            orig `eq` t2
        ]
    return [orig, t1, t2]

testRefreshTo :: IO ()
testRefreshTo = runTestSEB $ do
    orig <- fromVarsAsCells (direct A) ["b", "a"]
    --so the term listeners are placed
    v1 <- fromVarsAsCells (direct C) []
    cpy <- refreshVarsTbl B [("b",v1 :: TermId Cell)] orig
    return [orig, cpy]
{-
testRefreshBack :: IO ()
testRefreshBack = runTestSEB $ do
  --so the term listeners are placed
  orig <- fromVarsAsCells []
  v1 <- fromVarsAsCells []
  copy <- fromVarsAsCells [var v1, "a"]
  refreshVarsTbl [(CUSTOM "b",v1)] orig copy
  return [orig, copy]
-}

testRefreshUnification :: IO ()
testRefreshUnification = runTestSEB @(TermId Cell) $ do
  v1 <- fromVarsAsCells (direct A) []
  v2 <- fromVarsAsCells (direct B) []
  orig <- fromVarsAsCells (direct $ STR "orig")
    [["a", "a"], var v2]
  rule <- fromVarsAsCells (direct $ STR "rule")
    [["b", "a"], "b"]
  cpy <- refreshVarsTbl (STR "copy" ) [("b",v1 :: TermId Cell)] rule
  eq orig cpy
  return [rule, cpy, orig]


data TD = TD_A | TD_B | TD_C
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

data TC = Orig | TC Int deriving (Eq, Ord, Show)
instance Identifier TC (Domain TD)

test5 :: IO ()
test5 = flip runSEB (>> pure ()) $ do
    write (TC 2) [TD_A, TD_C]

    fork () $ \ lft -> watch (TC 2) () $ lft . write Orig

    watch Orig () $ \ _ -> write (TC 2) [TD_A]

    pure $ do
        v <- read Orig
        traceM $ show v
