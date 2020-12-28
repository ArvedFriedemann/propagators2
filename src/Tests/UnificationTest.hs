module Tests.UnificationTest where

import "base" Data.List
import "this" Data.Terms.TermFunctions
import "this" Control.Propagator
import "this" Control.Propagator.Conc
import "this" Control.Combinator.Logics

test1 :: IO ()
test1 = runTest $ do
    sv <- newEmptyCell "sv"
    sv_a <- fromVarsAsCells (ls [var sv, ccon "a"])
    b_sv <- fromVarsAsCells (ls [ccon "b", var sv])
    eq sv_a b_sv
    showAll [sv_a, b_sv, sv]

test2 :: IO ()
test2 = runTest $ do
    sv1 <- newEmptyCell "sv1"
    sv2 <- newEmptyCell "sv2"

    t1 <- fromVarsAsCells (ls [var sv1, ccon "a", var sv1])
    t2 <- fromVarsAsCells (ls [var sv2, ccon "a"])
    eq t1 t2
    showAll [t1, t2, sv1, sv2]

test3 :: IO ()
test3 = runTest $ do
    sv <- newEmptyCell "sv"
    t1 <- fromVarsAsCells (var sv)
    t2 <- fromVarsAsCells (ls [ccon "a", var sv])
    eq t1 t2
    showAll [t1, t2, sv]

test4 :: IO ()
test4 = runTest $ do
    sv1 <- newEmptyCell "sv1"

    orig <- fromVarsAsCells (ls [var sv1, ccon "a"])
    t1 <- fromVarsAsCells (ls [ccon "b", ccon "a"])
    t2 <- fromVarsAsCells (ls [ccon "b", ccon "b"])
    disjunctFork orig (void $ eq orig t1) (void $ eq orig t2)
    showAll [orig]

runTest :: Par String -> IO ()
runTest = (putStrLn =<<) . flip execPar pure

showAll :: (Monad m, PropagatorMonad m) => [TermCell m] -> m String
showAll = fmap (intercalate "\n\n") . traverse (fmap show . fromCellSize @String 100)
