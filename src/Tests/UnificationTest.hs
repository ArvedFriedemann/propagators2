module Tests.UnificationTest where

import "this" Data.Terms
import "this" Control.Propagator
--import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as S


test1 :: Either String String
test1 = runSimplePropagator $ do
  sv <- newEmptyCell @(TermSet SimplePropagator)
  t1 <- newCell $ TS $ S.singleton
    (VTerm (APPL (VVar sv) (VTerm (CON $ CUSTOM "a"))))
  t2 <- newCell $ TS $ S.singleton
    (VTerm (APPL (VTerm (CON $ CUSTOM "b")) (VVar sv) ))
  watch t1 termListener
  watch t2 termListener
  eq t1 t2
  rt1 <- readCell t1
  rt2 <- readCell t2
  return $ (show rt1) ++ "\n\n" ++ (show rt2)
