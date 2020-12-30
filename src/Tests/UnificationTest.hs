module Tests.UnificationTest where

import "base" GHC.Generics
import "base" Data.List
import "base" Data.Functor
import "base" Debug.Trace

import "deepseq" Control.DeepSeq

import "containers" Data.Set qualified as S

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Propagator
import "this" Control.Propagator.Conc
import "this" Control.Propagator.Event
import "this" Control.Combinator.Logics

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

testRefresh :: IO ()
testRefresh = runTestSEB $ do
  orig <- fromVarsAsCells (ls [ccon "b", ccon "a"])
  copy <- newEmptyCell "copy"
  v1 <- newEmptyCell "v1"
  refreshVarsTbl [(CUSTOM "b",v1)] orig copy
  return [orig, copy]

data TD = A | B | C
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)
instance NFData TD


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

runTestSEB :: SEB [TermCell SEB] -> IO ()
runTestSEB p = (putStrLn =<<) (runSEB p showAll)

runTest :: Par [TermCell Par] -> IO ()
runTest p = (putStrLn =<<) (execPar p showAll)

showAll :: (Monad m, PropagatorMonad m) => [TermCell m] -> m String
showAll = fmap (intercalate "\n\n") . traverse (fmap show . fromCellSize @String 100)
