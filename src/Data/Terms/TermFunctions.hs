{-# LANGUAGE NoImplicitPrelude #-}
module Data.Terms.TermFunctions where

import "base" Prelude hiding ( read )
import "base" Data.Foldable ( fold )
import "base" GHC.Exts

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

instance IsList (TermStruc a) where
    type Item (TermStruc a) = TermStruc a
    fromList = fold
    toList (SAPPL a b) = toList a ++ toList b
    toList a = pure a

instance IsString (TermStruc a) where
    fromString = SCON . CUSTOM

var :: a -> TermStruc (TermId a)
var = SVAR . Direct

data TermId p
    = Direct p
    | AppLeft (TermId p)
    | AppRight (TermId p)
  deriving (Eq, Ord, Show) 
instance (Ord p, Std p) => Identifier (TermId p) (TermSet (TermId p))

fromVarsAsCells :: (Ord p, Std p, MonadProp m) => p -> TermStruc (TermId p) -> m (TermId p)
fromVarsAsCells p = fromVarsAsCells' (Direct p)

fromVarsAsCells' :: (Ord p, Std p, MonadProp m) => TermId p -> TermStruc (TermId p) -> m (TermId p)
fromVarsAsCells' p SBOT = write p bot
fromVarsAsCells' p STOP = watchTerm p
fromVarsAsCells' p (SCON c) = do
    watchTerm p
    write p . constTerms $ c
fromVarsAsCells' _ (SVAR v) = pure v
fromVarsAsCells' p (SAPPL a b) = do
    watchTerm p
    ca <- fromVarsAsCells' (AppLeft p) a
    cb <- fromVarsAsCells' (AppRight p) b
    write p . appTerms $ (ca, cb)


fromCell :: (MonadProp m, Ord i, Std i) => TermId i -> m (TermStruc i)
fromCell c = read c >>= fromTermSet

fromCellSize :: (MonadProp m, Ord i, Std i) => Int -> TermId i -> m (TermStruc i)
fromCellSize s c = read c >>= fromTermSet' s

fromTermSet :: (MonadProp m, Ord i, Std i) => TermSet (TermId i) -> m (TermStruc i)
fromTermSet = fromTermSet' (-1)

fromTermSet' :: (MonadProp m, Ord i, Std i) => Int -> TermSet (TermId i) -> m (TermStruc i)
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
