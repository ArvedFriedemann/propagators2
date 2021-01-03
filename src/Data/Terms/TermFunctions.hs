{-# LANGUAGE NoImplicitPrelude #-}
module Data.Terms.TermFunctions where

import "base" Prelude hiding ( read )
import "base" Data.Functor

import "this" Data.Terms.Terms
import "this" Control.Propagator
import "this" Control.Util
import "this" Data.Lattice
import qualified "containers" Data.Set as S


data TermStruc a
    = STOP
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


data TermId p
    = Direct p
    | AppLeft (TermId p)
    | AppRight (TermId p)
  deriving (Eq, Ord, Show)
instance (Ord p, Std p) => Identifier (TermId p) (TermSet (TermId p))

fromVarsAsCells :: (Ord p, Std p, MonadProp m) => TermId p -> TermStruc (TermId p) -> m (TermId p)
fromVarsAsCells p SBOT = write p bot $> p
fromVarsAsCells p STOP = watchTerm p $> p
fromVarsAsCells p (SCON c) = do
    watchTerm p
    write p (termSetWithConstants $ S.singleton c)
    pure p
fromVarsAsCells _ (SVAR v) = pure v
fromVarsAsCells p (SAPPL a b) = do
  watchTerm p
  ca <- fromVarsAsCells (AppLeft p) a
  cb <- fromVarsAsCells (AppRight p) b
  write p (termSetWithApls $ S.singleton (ca, cb))
  pure p


fromCell :: (MonadProp m, Ord i, Std i) => TermId i -> m (TermStruc i)
fromCell c = read c >>= fromTermSet

fromCellSize :: (MonadProp m, Ord i, Std i) => Int -> TermId i -> m (TermStruc i)
fromCellSize s c = read c >>= fromTermSet' s

fromTermSet :: (MonadProp m, Ord i, Std i) => TermSet (TermId i) -> m (TermStruc i)
fromTermSet = fromTermSet' (-1)

fromTermSet' :: (MonadProp m, Ord i, Std i) => Int -> TermSet (TermId i) -> m (TermStruc i)
fromTermSet' 0 _ = pure STOP
fromTermSet' _ TSBot = pure SBOT
fromTermSet' n ts
    | ts == top = pure STOP
    | not . null . constants $ ts = do
        pure . con . head . S.toList . constants $ ts
    | not $ null (applications ts) = do
        (a,b) <- pure . head . S.toList . applications $ ts
        a' <- fromCellSize (n-1) a
        b' <- fromCellSize (n-1) b
        pure $ SAPPL a' b'--applts a' b' --if a variable is assigned top, it would just vanish
    | otherwise = fromCellSize (n-1) . head . S.toList . variables $ ts --TODO: This will recurse if there are cyclic equalities
