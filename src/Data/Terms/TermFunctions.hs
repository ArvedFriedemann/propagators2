module Data.Terms.TermFunctions where

import "this" Data.Terms.Terms
import "this" Control.Propagator
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

listToStruc :: [TermStruc a] -> TermStruc a
listToStruc lst = foldl applts STOP lst

fromVarsAsCells :: (PropagatorMonad m) => TermStruc (Cell m (TermSet m)) -> m (Cell m (TermSet m))
fromVarsAsCells STOP = newEmptyCell "mpt_trm"
fromVarsAsCells (SCON c) = newCell "cnst" (TS $ S.singleton (VTerm $ CON c))
fromVarsAsCells (SVAR v) = return v
fromVarsAsCells (SAPPL a b) = do
  ca <- fromVarsAsCells a
  cb <- fromVarsAsCells b
  newCell "appl" (TS $ S.singleton (VTerm $ APPL ca cb))
