module Tests.UnificationTest where

import "base" Control.Monad

--import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Propagator
import "this" Control.Propagator.Conc
--import "base" Data.Either
--import "containers" Data.Set ( Set )
--import qualified "containers" Data.Set as S
import "this" Control.Combinator.Logics


test1 :: IO ()
test1 = (putStrLn =<<) $ flip execPar printMyStuff $ do
    sv <- newEmptyCell "sv"
    sv_a <- fromVarsAsCells (ls [var sv, ccon "a"])
    b_sv <- fromVarsAsCells (ls [ccon "b", var sv])
    eq sv_a b_sv
    return (sv_a, b_sv, sv)
  where
    printMyStuff (sv_a, b_sv, sv) = do
        rsv_a <- readCell sv_a--fromCell' sv_a
        rb_sv <- readCell b_sv--fromCell' b_sv
        rsv <- readCell sv--fromCell' sv
        pure $ (show rsv_a) ++ "\n\n" ++ (show rb_sv) ++ "\n\n" ++ (show rsv)

test2 :: IO ()
test2 = (putStrLn =<<) $ flip execPar printMyStuff $ do
    sv1 <- newEmptyCell "sv1"
    sv2 <- newEmptyCell "sv2"

    t1 <- fromVarsAsCells (ls [var sv1, ccon "a", var sv1])
    t2 <- fromVarsAsCells (ls [var sv2, ccon "a"])
    eq t1 t2
    return (t1, t2, sv1, sv2)
  where
    printMyStuff (t1, t2, sv1, sv2) = do
        rt1 <- fromCell' t1
        rt2 <- fromCell' t2
        rsv1 <- fromCell' sv1
        rsv2 <- fromCell' sv2
        pure $ (show rt1) ++ "\n\n"
            ++ (show rt2) ++ "\n\n"
            ++ (show rsv1) ++ "\n\n"
            ++ (show rsv2)

test3 :: IO ()
test3 = (putStrLn =<<) $ flip execPar printMyStuff $ do
    sv <- newEmptyCell "sv"
    t1 <- fromVarsAsCells (var sv)
    t2 <- fromVarsAsCells (ls [ccon "a", var sv])
    eq t1 t2
    return (t1, t2, sv)
  where
    printMyStuff (t1, t2, sv) = do
        rt1 <- fromCellSize' 100 t1
        rt2 <- fromCellSize' 100 t2
        rsv <- fromCellSize' 100 sv
        pure $ (show rt1) ++ "\n\n"
            ++ (show rt2) ++ "\n\n"
            ++ (show rsv)

test4 :: IO ()
test4 = (putStrLn =<<) $ flip execPar printMyStuff $ do
    sv1 <- newEmptyCell "sv1"

    orig <- fromVarsAsCells (ls [var sv1, ccon "a"])
    t1 <- fromVarsAsCells (ls [ccon "b", ccon "a"])
    t2 <- fromVarsAsCells (ls [ccon "b", ccon "b"])
    disjunctFork orig (void $ eq orig t1) (void $ eq orig t2)
    return orig
  where
    printMyStuff orig = do
        rorig <- fromCell' orig
        pure $ (show rorig)
