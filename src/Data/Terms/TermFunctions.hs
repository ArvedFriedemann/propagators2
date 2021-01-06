{-# LANGUAGE NoImplicitPrelude #-}
module Data.Terms.TermFunctions where

import "base" Prelude hiding ( read )
import "base" Data.Foldable ( fold )
import "base" GHC.Exts
import "base" Data.Functor

import "this" Data.Terms.Terms
import "this" Control.Propagator
import "this" Data.Lattice
import qualified "containers" Data.Set as S


data TermStruc a
    = STOP
    | SBOT
    | SCON TermConst
    | SVAR a
    | SAPPL (TermStruc a) (TermStruc a)
  deriving (Eq, Ord, Functor)

instance Show a => Show (TermStruc a) where
    showsPrec _ STOP = showString "top"
    showsPrec _ SBOT = showString "bot"
    showsPrec _ (SCON (CUSTOM s)) = showString s
    showsPrec n (SCON c) = showsPrec n c
    showsPrec n (SVAR v) = showsPrec n v
    showsPrec n (SAPPL s c@(SAPPL _ _)) = (showString "(").(showsPrec n s).(showString ")").(showsPrec n c)
    showsPrec n (SAPPL s c) = (showsPrec n s).(showString " ").(showsPrec n c)

instance Semigroup (TermStruc a) where
    STOP <> a = a
    a <> STOP = a
    a <> b = SAPPL a b
instance Monoid (TermStruc a) where
    mempty = STOP

--TODO: use a reader!
instance IsList (TermStruc a) where
    type Item (TermStruc a) = TermStruc a
    fromList [] = STOP
    fromList lst = foldl1 SAPPL lst
    toList  = pure

instance IsString (TermStruc a) where
    fromString = SCON . CUSTOM

var :: a -> TermStruc a
var = SVAR

class PosTermId i where
  appLeft :: i -> i
  appRight :: i -> i



fromVarsAsCells ::
  (Ord i, Std i, MonadProp m,
  Identifier i (TermSet i),
  PosTermId i) =>
  i -> TermStruc i -> m i
fromVarsAsCells p SBOT = write p bot $> p
fromVarsAsCells p STOP = watchTerm p $> p
fromVarsAsCells p (SCON c) = do
    watchTerm p
    write p (constTerm c)
    pure p
fromVarsAsCells _ (SVAR v) = pure v
fromVarsAsCells p (SAPPL a b) = do
  watchTerm p
  ca <- fromVarsAsCells (appLeft p) a
  cb <- fromVarsAsCells (appRight p) b
  write p (aplTerm (ca, cb))
  pure p


fromCell :: (MonadProp m, Ord i, Std i,
  Identifier i (TermSet i)) => i -> m (TermStruc i)
fromCell c = read c >>= fromTermSet

fromCellSize :: (MonadProp m, Ord i, Std i,
  Identifier i (TermSet i)) => Int -> i -> m (TermStruc i)
fromCellSize s c = read c >>= fromTermSet' s

fromTermSet :: (MonadProp m, Ord i, Std i,
  Identifier i (TermSet i)) => TermSet i -> m (TermStruc i)
fromTermSet = fromTermSet' (-1)

fromTermSet' :: (MonadProp m, Ord i, Std i,
  Identifier i (TermSet i)) =>
  Int -> TermSet i -> m (TermStruc i)
fromTermSet' 0 _ = pure STOP
fromTermSet' _ Bot = pure SBOT
fromTermSet' _ Top = pure STOP
fromTermSet' _ (TS (Just c) _ _) = pure . SCON $ c
fromTermSet' n ts
    | not $ null (applications ts) = do
        (a,b) <- pure . head . S.toList . applications $ ts
        a' <- fromCellSize (n-1) a
        b' <- fromCellSize (n-1) b
        pure $ SAPPL a' b'--applts a' b' --if a variable is assigned top, it would just vanish
    | otherwise = fromCellSize (n-1) . head . S.toList . variables $ ts --TODO: This will recurse if there are cyclic equalities
