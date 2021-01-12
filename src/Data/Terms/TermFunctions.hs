{-# LANGUAGE NoImplicitPrelude #-}
module Data.Terms.TermFunctions where

import "base" Prelude hiding ( read )
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
instance HasTop (TermStruc a) where
    top = STOP
    isTop STOP = True
    isTop _ = False
instance HasBot (TermStruc a) where
    bot = SBOT
    isBot SBOT = True
    isBot _ = False


instance Show a => Show (TermStruc a) where
    showsPrec _ STOP = showString "top"
    showsPrec _ SBOT = showString "bot"
    showsPrec _ (SCON (CUST s)) = showString s
    showsPrec n (SCON c) = showsPrec n c
    showsPrec n (SVAR v) = (showString "(@").(showsPrec n v).(showString "@)")
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
    fromString = SCON . CUST

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
fromCell c = read c >>= fromTermSet c

fromCellSize :: (MonadProp m, Ord i, Std i,
  Identifier i (TermSet i)) => Int -> i -> m (TermStruc i)
fromCellSize s c = read c >>= fromTermSet' s c

fromTermSet :: (MonadProp m, Ord i, Std i,
  Identifier i (TermSet i)) => i -> TermSet i -> m (TermStruc i)
fromTermSet = fromTermSet' (-1)

fromTermSet' :: (MonadProp m, Ord i, Std i,
  Identifier i (TermSet i)) =>
  Int -> i -> TermSet i -> m (TermStruc i)
fromTermSet' 0 _ _ = pure (SCON $ CUST $ "(...)")--pure STOP
fromTermSet' _ _ Bot = pure SBOT
fromTermSet' _ c Top = pure (SCON $ CUST $ "(@ "++show c++" @)")--pure STOP
fromTermSet' _ _ (TS (Just c) _ _) = pure . SCON $ c
fromTermSet' n _ ts
    | not $ null (applications ts) = do
        (a,b) <- pure . head . S.toList . applications $ ts
        a' <- fromCellSize (n-1) a
        b' <- fromCellSize (n-1) b
        pure $ SAPPL a' b'--applts a' b' --if a variable is assigned top, it would just vanish
    | not $ null (variables ts) = do
        --here we can safely assume that there cannot be a term hanging on this variable. If there was, it would have propagated here. Therefore this always is a dangling variable, so we just return the smallest variable of the equality cluster (so that all cluster show the same variable)
        pure . SVAR . head . S.toList . variables $ ts
    | otherwise = pure Top
