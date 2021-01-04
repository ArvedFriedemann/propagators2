{-# LANGUAGE NoImplicitPrelude #-}
module Tests.UnificationTest where

import "base" Prelude hiding ( read )
import "base" GHC.Generics
import "base" Data.List
import "base" Debug.Trace

import "this" Data.Terms.TermFunctions
import "this" Data.Domain
import "this" Control.Propagator
import "this" Control.Propagator.Event
import "this" Control.Combinator.Logics
import "this" Control.Propagator.Event


data Cell = Sv Int | A | B | C deriving (Eq, Ord, Show)

test1 :: IO ()
test1 = runTestSEB $ do
    sv_a <- fromVarsAsCells A $ var (Sv 0) <> "a"
    b_sv <- fromVarsAsCells B $ "b" <> var (Sv 0)
    eq sv_a b_sv
    return [sv_a, b_sv, Direct (Sv 0)]

test2 :: IO ()
test2 = runTestSEB $ do
    t1 <- fromVarsAsCells A [var $ Sv 1, "a", var $ Sv 1]
    t2 <- fromVarsAsCells B $ var (Sv 2) <> "a"
    eq t1 t2
    return [t1, t2, Direct $ Sv 1, Direct $ Sv 2]


test3 :: IO ()
test3 = runTestSEB $ do
    t1 <- fromVarsAsCells A $ var $ Sv 0
    t2 <- fromVarsAsCells B $ "a" <> var (Sv 0)
    eq t1 t2
    return [t1, t2, Direct $ Sv 0]

test4 :: IO ()
test4 = runTestSEB $ do
    orig <- fromVarsAsCells A $ var (Sv 1) <> "A"
    t1 <- fromVarsAsCells B $ "B" <> "A"
    t2 <- fromVarsAsCells C $ "B" <> "B"

    disjunctFork orig
        [ do
            watch orig () (\r -> (show <$> fromTermSet r) >>= (\r' -> traceM $ "branch A:" ++ (show r')) )
            eq orig t1
        , do
            watch orig () (\r -> (show <$> fromTermSet r) >>= (\r' -> traceM $ "branch B:" ++ (show r')) )
            eq orig t2
        ]
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
  copy <- fromVarsAsCells (ls [])
  refreshVarsTbl [(CUSTOM "b",v1)] rule copy
  eq orig copy
  return [rule, copy, orig]

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
