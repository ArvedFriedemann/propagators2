module Tests.UnificationTest where

--import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Propagator
import "base" Data.Either
--import "containers" Data.Set ( Set )
--import qualified "containers" Data.Set as S


test1 :: IO ()
test1 = putStrLn $ fromRight "Error" $ runSimplePropagator $ do
  sv <- newEmptyCell "sv"
  sv_a <- fromVarsAsCells (ls [var sv, ccon "a"])
  b_sv <- fromVarsAsCells (ls [ccon "b", var sv])
  eq sv_a b_sv
  rsv_a <- readCell sv_a
  rb_sv <- readCell b_sv
  rsv <- readCell sv
  return $ (show rsv_a) ++ "\n\n" ++ (show rb_sv) ++ "\n\n" ++ (show rsv)

{-
test2 :: IO ()
test2 = putStrLn $ fromRight "Error" $ runSimplePropagator $ do
  sv1 <- newEmptyCell @(TermSet SimplePropagator) "sv1"
  sv2 <- newEmptyCell @(TermSet SimplePropagator) "sv2"
  t1 <- newEmptyCell @(TermSet SimplePropagator) "t1"
  t2 <- newEmptyCell @(TermSet SimplePropagator) "t2"

  write t1 $ TS $ S.singleton
    (VTerm (APPL (VVar sv1) (VTerm $ APPL
      (VTerm $ CON $ CUSTOM "a")
      (VVar sv1)
      )))
  write t2 $ TS $ S.singleton
    (VTerm (APPL (VTerm (CON $ CUSTOM "b")) (VVar sv2) ))
  watch t1 $ termListener t1
  watch t2 $ termListener t2
  watch sv1 $ termListener sv1
  watch sv2 $ termListener sv2
  eq t1 t2
  eq t2 t1
  rt1 <- readCell t1
  rt2 <- readCell t2
  rv1 <- readCell sv1
  rv2 <- readCell sv2
  return $ (show rt1) ++ "\n\n" ++ (show rt2) ++ "\n\n" ++
          (show rv1) ++ "\n\n" ++ (show rv2)
-}
