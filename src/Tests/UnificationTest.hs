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
  rsv_a <- fromCell' sv_a
  rb_sv <- fromCell' b_sv
  rsv <- fromCell' sv
  return $ (show rsv_a) ++ "\n\n" ++ (show rb_sv) ++ "\n\n" ++ (show rsv)


test2 :: IO ()
test2 = putStrLn $ fromRight "Error" $ runSimplePropagator $ do
  sv1 <- newEmptyCell "sv1"
  sv2 <- newEmptyCell "sv2"

  t1 <- fromVarsAsCells (ls [var sv1, ccon "a", var sv1])
  t2 <- fromVarsAsCells (ls [var sv2, ccon "a"])
  eq t1 t2
  rt1 <- fromCell' t1
  rt2 <- fromCell' t2
  rv1 <- fromCell' sv1
  rv2 <- fromCell' sv2
  return $ (show rt1) ++ "\n\n" ++ (show rt2) ++ "\n\n" ++
          (show rv1) ++ "\n\n" ++ (show rv2)
