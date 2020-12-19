module Data.Terms.TermFunctions where

import "this" Data.Terms.Terms
import "this" Control.Propagator
import "this" Control.Util
--import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as S

data TermStruc a = STOP
                | SCON TermConst
                | SVAR a
                | SAPPL (TermStruc a) (TermStruc a)

applts :: TermStruc a -> TermStruc a -> TermStruc a
applts STOP a = a
applts a STOP = a
applts a b = SAPPL a b

con :: TermConst -> TermStruc a
con = SCON

ccon :: String -> TermStruc a
ccon s = SCON (CUSTOM s)

var :: a -> TermStruc a
var a = SVAR a

ls :: [TermStruc a] -> TermStruc a
ls lst = foldl applts STOP lst

fromVarsAsCells :: (PropagatorEqMonad m) => TermStruc (Cell m (TermSet m)) -> m (Cell m (TermSet m))
fromVarsAsCells STOP =  newEmptyCell "mpt_trm" <**< watchTerm
fromVarsAsCells (SCON c) = newCell "cnst" (TS $ S.singleton (VTerm $ CON c)) <**< watchTerm
fromVarsAsCells (SVAR v) = return v
fromVarsAsCells (SAPPL a b) = do
  ca <- fromVarsAsCells a
  cb <- fromVarsAsCells b
  newCell "appl" (TS $ S.singleton (VTerm $ APPL ca cb)) <**< watchTerm
