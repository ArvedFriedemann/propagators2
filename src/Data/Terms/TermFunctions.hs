{-# LANGUAGE NoImplicitPrelude #-}
module Data.Terms.TermFunctions where

import "base" Prelude hiding ( read )
import "base" GHC.Exts
import "base" Data.Functor
import "base" Data.Typeable

import "transformers" Control.Monad.Trans.Writer

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

import "this" Data.Terms.Terms
import "this" Data.Lattice
import "this" Control.Propagator.Class
import "this" Control.Combinator



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
    --TODO: Should not be used when variables are not strings
    showsPrec n (SVAR v) = (showsPrec n v)
    --showsPrec n (SVAR v) = (showString "@").(showsPrec n v).(showString "@")
    showsPrec n (SAPPL s c@(SAPPL _ _)) = (showsPrec n s).(showString "(").(showsPrec n c).(showString ")")
    --showsPrec n (SAPPL s@(SAPPL _ _) c) = (showString "(").(showsPrec n s).(showString ")").(showsPrec n c)
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

tsVars :: TermStruc a -> [a]
tsVars = execWriter . tsVars'
tsVars' :: TermStruc a -> Writer [a] ()
tsVars' (SVAR s) = tell [s]
tsVars' (SAPPL a b) = tsVars' a >> tsVars' b
tsVars' _ = return ()

exchangeVars :: (a -> TermStruc b) -> TermStruc a -> TermStruc b
exchangeVars fkt (SVAR v) = fkt v
exchangeVars fkt (SAPPL a b) = SAPPL (exchangeVars fkt a) (exchangeVars fkt b)
exchangeVars _ STOP = STOP
exchangeVars _ SBOT = SBOT
exchangeVars _ (SCON c) = SCON c

apls :: TermStruc a -> TermStruc a -> TermStruc a
apls x STOP = x
apls STOP x = x
apls x y = SAPPL x y

stdlst :: [TermStruc a] -> TermStruc a
stdlst = foldl apls STOP

data PosTermId v = AppLeft | AppRight
  deriving (Show, Eq, Ord)
instance Identifier (PosTermId v) (TermSet (TermSetPtr v))

fromVarsAsCells :: forall m v scope i j.
  ( MonadProp m v scope, Std i, Std j, Typeable v, StdPtr v
  , Identifier i (TermSet (TermSetPtr v))
  , Identifier j (TermSet (TermSetPtr v))) =>
  i -> TermStruc j -> m (TermSetPtr v)
fromVarsAsCells name ts = do
  p <- new name
  fromVarsAsCells' (TSP p) ts

fromVarsAsCells' :: forall m v scope j.
  ( MonadProp m v scope, Std j, Typeable v, StdPtr v
  , Identifier j (TermSet (TermSetPtr v))) =>
  TermSetPtr v -> TermStruc j -> m (TermSetPtr v)
fromVarsAsCells' (TSP p) SBOT = do
  write p bot $> (TSP p)
fromVarsAsCells' (TSP p) STOP = do
  watchTerm (TSP p) $> (TSP p)
fromVarsAsCells' (TSP p) (SCON c) = do
    watchTerm (TSP p)
    write p (constTerm c) $> (TSP p)
fromVarsAsCells' (TSP p) (SVAR v) = do
  --TODO: slightly inefficient
  vr <- new v
  eq vr p $> (TSP vr)
fromVarsAsCells' (TSP p) (SAPPL a b) = do
    watchTerm (TSP p)
    left  <- TSP <$> newRelative p  (AppLeft @v)
    right <- TSP <$> newRelative p (AppRight @v)
    ca <- fromVarsAsCells' left a
    cb <- fromVarsAsCells' right b
    write p (aplTerm (ca, cb)) $> (TSP p)



fromCell :: (MonadProp m v scope, StdPtr v) => TermSetPtr v -> m (TermStruc (TermSetPtr v))
fromCell (TSP c) = read c >>= fromTermSet (TSP c)

fromCellSize :: (MonadProp m v scope, StdPtr v) => Int -> TermSetPtr v -> m (TermStruc (TermSetPtr v))
fromCellSize s (TSP c) = read c >>= fromTermSet' s (TSP c)

fromTermSet :: (MonadProp m v scope, StdPtr v) => TermSetPtr v -> TermSet (TermSetPtr v) -> m (TermStruc (TermSetPtr v))
fromTermSet = fromTermSet' (-1)

fromTermSet' :: (MonadProp m v scope, StdPtr v) =>
  Int -> TermSetPtr v -> TermSet (TermSetPtr v) -> m (TermStruc (TermSetPtr v))
fromTermSet' 0 _ _ = pure (SCON $ CUST $ "(...)")--pure STOP
fromTermSet' _ _ ts | isBot ts = pure SBOT
fromTermSet' _ c Top = pure (SCON $ CUST $ "(@ "++show c++" @)")--pure STOP
fromTermSet' _ _ (TS (Set.toList -> [c]) _ _ _) = pure . SCON $ c
fromTermSet' n _ ts
    | not $ null (applications ts) = do
        (a,b) <- pure . head . Set.toList . applications $ ts
        a' <- fromCellSize (n-1) a
        b' <- fromCellSize (n-1) b
        pure $ SAPPL a' b'--applts a' b' --if a variable is assigned top, it would just vanish
    | not $ null (variables ts) = do
        --here we can safely assume that there cannot be a term hanging on this variable. If there was, it would have propagated here. Therefore this always is a dangling variable, so we just return the smallest variable of the equality cluster (so that all cluster show the same variable)
        pure . SVAR . head . Set.toList . variables $ ts
    | otherwise = pure Top





--
