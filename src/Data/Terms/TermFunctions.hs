module Data.Terms.TermFunctions where

import "this" Data.Terms.Terms
import "this" Control.Propagator
import "this" Control.Util
--import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as S


type TermCell m = Cell m (TermSet m)

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

fromVarsAsCells :: (Monad m, PropagatorMonad m) => TermStruc (TermCell m) -> m (TermCell m)
fromVarsAsCells SBOT =  newEmptyCell "mpt_trm" <**< watchTerm <**< (flip write) TSBot
fromVarsAsCells STOP =  newEmptyCell "mpt_trm" <**< watchTerm
fromVarsAsCells (SCON c) = newCell "cnst" (termSetWithConstants $ S.singleton (VTerm $ CON c)) <**< watchTerm
fromVarsAsCells (SVAR v) = pure v
fromVarsAsCells (SAPPL a b) = do
  ca <- fromVarsAsCells a
  cb <- fromVarsAsCells b
  newCell "appl" (termSetWithApls $ S.singleton (VTerm $ APPL ca cb)) <**< watchTerm


fromCell :: forall a m. (Monad m, PropagatorMonad m) => TermCell m -> m (TermStruc a)
fromCell c = readCell c >>= fromTermSet

fromCellSize :: forall a m. (Monad m, PropagatorMonad m) => Int -> TermCell m -> m (TermStruc a)
fromCellSize s c = readCell c >>= fromTermSet' s

fromTermSet :: (Monad m, PropagatorMonad m) => TermSet m -> m (TermStruc a)
fromTermSet = fromTermSet' (-1)

fromTermSet' :: (Monad m, PropagatorMonad m) => Int -> TermSet m -> m (TermStruc a)
fromTermSet' 0 _ = pure STOP
fromTermSet' _ TSBot = pure SBOT
fromTermSet' n ts
  | ts == emptyTermSet = pure STOP
  | not $ null (constants ts) = do
    pure $ con $ head $ constantContents (S.toList $ constants ts)
  | not $ null (applications ts) = do
    (a,b) <- pure $ head $ applContents (S.toList $ applications ts)
    a' <- fromCellSize (n-1) a
    b' <- fromCellSize (n-1) b
    pure $ applts a' b'
  | otherwise = fromCellSize (n-1) $ head $ variableContents (S.toList $ variables ts) --TODO: This will recurse if there are cyclic equalities
