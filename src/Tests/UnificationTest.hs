module Tests.UnificationTest where

import "containers" Data.Set qualified as S
import "base" Data.Functor
import "base" Debug.Trace

import "this" Tests.TestLogic

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions

import "this" Control.Propagator.Class
import "this" Control.Combinator.Logics
import "this" Control.Propagator.Event

test1 :: IO ()
test1 = runTest $ do
    sv <- newEmptyCell "sv"
    sv_a <- fromVarsAsCells (ls [var sv, ccon "a"])
    b_sv <- fromVarsAsCells (ls [ccon "b", var sv])
    eq sv_a b_sv
    return [sv_a, b_sv, sv]

test2 :: IO ()
test2 = runTest $ do
    sv1 <- newEmptyCell "sv1"
    sv2 <- newEmptyCell "sv2"

    t1 <- fromVarsAsCells (ls [var sv1, ccon "a", var sv1])
    t2 <- fromVarsAsCells (ls [var sv2, ccon "a"])
    eq t1 t2
    return [t1, t2, sv1, sv2]


test3 :: IO ()
test3 = runTest $ do
    sv <- newEmptyCell "sv"
    t1 <- fromVarsAsCells (var sv)
    t2 <- fromVarsAsCells (ls [ccon "a", var sv])
    eq t1 t2
    return [t1, t2, sv]

test4 :: IO ()
test4 = runTestSEB $ do
    sv1 <- newEmptyCell "sv1"

    orig <- fromVarsAsCells (ls [var sv1, ccon "a"])
    t1 <- fromVarsAsCells (ls [ccon "b", ccon "a"])
    t2 <- fromVarsAsCells (ls [ccon "b", ccon "b"])

    disjunctFork orig
      (void $ do
        watch orig (\r -> (show <$> fromTermSetString r) >>= (\r' -> traceM $ "branch A:" ++ (show r')) )
        eq orig t1 )
      (void $ do
        watch orig (\r -> (show <$> fromTermSetString r) >>= (\r' -> traceM $ "branch B:" ++ (show r')) )
        eq orig t2 )
    return [orig, t1, t2]

testRefreshTo :: IO ()
testRefreshTo = runTestSEB $ do
  orig <- fromVarsAsCells (ls [ccon "b", ccon "a"])
  --so the term listeners are placed
  copy <- fromVarsAsCells (ls [])
  v1 <- fromVarsAsCells (ls [])
  refreshVarsTbl [(CUSTOM "b",v1)] orig copy
  return [orig, copy]

testRefreshBack :: IO ()
testRefreshBack = runTestSEB $ do
  --so the term listeners are placed
  orig <- fromVarsAsCells (ls [])
  v1 <- fromVarsAsCells (ls [])
  copy <- fromVarsAsCells (ls [var v1, ccon "a"])
  refreshVarsTbl [(CUSTOM "b",v1)] orig copy
  return [orig, copy]

testRefreshUnification :: IO ()
testRefreshUnification = runTestSEB $ do
  v1 <- fromVarsAsCells (ls [])
  v2 <- fromVarsAsCells (ls [])
  orig <- fromVarsAsCells (ls [ls [ccon "a", ccon "a"], var v2])
  rule <- fromVarsAsCells (ls [ls [ccon "b", ccon "a"], ccon "b"])
  rulecpy <- fromVarsAsCells (ls [ls [var v1, ccon "a"], var v1])
  copy <- fromVarsAsCells (ls [])
  --TODO: somehow, when adding the copy, something throws bot! Probably when the copied term is unified or something
  refreshVarsTbl [(CUSTOM "b",v1)] rule copy
  eq orig rulecpy
  return [rule, rulecpy, copy, orig]

data TD = A | B | C
  deriving (Eq, Ord, Show, Enum, Bounded)


test5 :: IO ()
test5 = flip runSEB (>> pure ()) $ do
    orig <- newCell "orig" ([A, B, C] :: S.Set TD)
    c2 <- newCell "c2" ([A, C] :: S.Set TD)

    orig `eq` c2

    namedFork "Fork" $ \ lft -> do
        orig `eq` c2
        write c2 [A]
        watch orig $ lft . write orig
        pure ()


    pure $ do
        v <- readCell orig
        traceM $ show v
