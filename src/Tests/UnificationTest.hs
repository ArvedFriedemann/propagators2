module Tests.UnificationTest where

import "this" Data.Terms
import "this" Control.Propagator
import "base" Data.Either
--import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as S


test1 :: IO ()
test1 = putStrLn $ fromRight "Error" $ runSimplePropagator $ do
  sv <- newEmptyCell @(TermSet SimplePropagator)
  t1 <- newCell $ TS $ S.singleton
    (VTerm (APPL (VVar sv) (VTerm (CON $ CUSTOM "a"))))
  t2 <- newCell $ TS $ S.singleton
    (VTerm (APPL (VTerm (CON $ CUSTOM "b")) (VVar sv) ))
  watch t1 termListener
  watch t2 termListener
  watch sv termListener
  eq t1 t2
  eq t2 t1
  rt1 <- readCell t1
  rt2 <- readCell t2
  return $ (show rt1) ++ "\n\n" ++ (show rt2)

test2 :: IO ()
test2 = putStrLn $ fromRight "Error" $ runSimplePropagator $ do
  sv1 <- newEmptyCell @(TermSet SimplePropagator)
  sv2 <- newEmptyCell @(TermSet SimplePropagator)
  t1 <- newEmptyCell @(TermSet SimplePropagator)
  t2 <- newEmptyCell @(TermSet SimplePropagator)

  write t1 $ TS $ S.singleton
    (VTerm (APPL (VVar sv1) (VTerm $ APPL
      (VTerm $ CON $ CUSTOM "a")
      (VVar sv1)
      )))
  write t2 $ TS $ S.singleton
    (VTerm (APPL (VTerm (CON $ CUSTOM "b")) (VVar sv2) ))
  watch t1 termListener
  watch t2 termListener
  watch sv1 termListener
  watch sv2 termListener
  eq t1 t2
  eq t2 t1
  rt1 <- readCell t1
  rt2 <- readCell t2
  rv1 <- readCell sv1
  rv2 <- readCell sv2
  return $ (show rt1) ++ "\n\n" ++ (show rt2) ++ "\n\n" ++
          (show rv1) ++ "\n\n" ++ (show rv2)
