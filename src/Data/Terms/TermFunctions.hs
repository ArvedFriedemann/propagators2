module Data.Terms.TermFunctions where

import "this" Data.Terms.Terms
import "this" Control.Propagator
import "this" Control.Util
--import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as S

data TermStruc a = STOP
                | SBOT
                | SCON TermConst
                | SVAR a
                | SAPPL (TermStruc a) (TermStruc a)
    deriving (Eq, Ord)

instance (Show a) => Show (TermStruc a) where
  showsPrec _ STOP = showString "top"
  showsPrec _ SBOT = showString "bot"
  showsPrec _ (SCON (CUSTOM s)) = showString s
  showsPrec n (SCON c) = showsPrec n c
  showsPrec n (SVAR v) = showsPrec n v
  showsPrec n (SAPPL s c@(SAPPL _ _)) = (showString "(").(showsPrec n s).(showString ")").(showsPrec n c)
  showsPrec n (SAPPL s c) = (showsPrec n s).(showString " ").(showsPrec n c)


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
fromVarsAsCells SBOT =  newEmptyCell "mpt_trm" <**< watchTerm <**< (flip write) TSBot
fromVarsAsCells STOP =  newEmptyCell "mpt_trm" <**< watchTerm
fromVarsAsCells (SCON c) = newCell "cnst" (termSetWithConstants $ S.singleton (VTerm $ CON c)) <**< watchTerm
fromVarsAsCells (SVAR v) = return v
fromVarsAsCells (SAPPL a b) = do
  ca <- fromVarsAsCells a
  cb <- fromVarsAsCells b
  newCell "appl" (termSetWithApls $ S.singleton (VTerm $ APPL ca cb)) <**< watchTerm


fromCell :: (PropagatorMonad m) => Cell m (TermSet m) -> m (TermStruc a)
fromCell c = readCell c >>= fromTermSet

fromCellSize :: (PropagatorMonad m) => Int -> Cell m (TermSet m) -> m (TermStruc a)
fromCellSize s c = readCell c >>= fromTermSet' s

--Just for the type, so casting can be ommitted
fromCellSize' :: (PropagatorMonad m) => Int -> Cell m (TermSet m) -> m (TermStruc String)
fromCellSize' = fromCellSize

--Just for the type, so casting can be ommitted
fromCell' :: (PropagatorMonad m) => Cell m (TermSet m) -> m (TermStruc String)
fromCell' = fromCell

fromTermSet :: (PropagatorMonad m) => TermSet m -> m (TermStruc a)
fromTermSet = fromTermSet' (-1)

fromTermSet' :: (PropagatorMonad m) => Int -> TermSet m -> m (TermStruc a)
fromTermSet' 0 _ = return STOP
fromTermSet' _ TSBot = return SBOT
fromTermSet' n ts
  | ts == emptyTermSet = return STOP
  | not $ null (constants ts) = do
    return $ con $ head $ constantContents (S.toList $ constants ts)
  | not $ null (applications ts) = do
    (a,b) <- return $ head $ applContents (S.toList $ applications ts)
    a' <- fromCellSize (n-1) a
    b' <- fromCellSize (n-1) b
    return $ applts a' b'
  | otherwise = fromCellSize (n-1) $ head $ variableContents (S.toList $ variables ts) --TODO: This will recurse if there are cyclic equalities
