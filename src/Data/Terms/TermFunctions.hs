{-# LANGUAGE NoImplicitPrelude #-}
module Data.Terms.TermFunctions where

import "base" Prelude hiding ( read )
import "base" GHC.Exts
import "base" Data.Functor
import "base" Debug.Trace

import "transformers" Control.Monad.Trans.Writer

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


------------------------------
--Reverse Parsing
------------------------------
lassocOp :: (Eq a) => TermStruc a -> TermStruc a -> [TermStruc a]
lassocOp op = lassocOpF (==op)
lassocOpF :: (Eq a) => (TermStruc a -> Bool) -> TermStruc a -> [TermStruc a]
lassocOpF opf t = execWriter (lassocOp' opf t)
lassocOp' :: (Eq a) => (TermStruc a -> Bool) -> TermStruc a -> Writer [TermStruc a] ()
lassocOp' opf (SAPPL (SAPPL x op') y)
  | opf op' = lassocOp' opf x >> tell [y]
lassocOp' _ t = tell [t]

rassocOp :: (Eq a) => TermStruc a -> TermStruc a -> [TermStruc a]
rassocOp op = rassocOpF (==op)
rassocOpF :: (Eq a) => (TermStruc a -> Bool) -> TermStruc a -> [TermStruc a]
rassocOpF opf (SAPPL x (SAPPL op' y))
  | opf op' = x : (rassocOpF opf y)
rassocOpF _ t = [t]

removeLrecBrackets :: (Eq a) => TermStruc a -> TermStruc a -> TermStruc a -> TermStruc a
removeLrecBrackets on off = removeLrecBracketsF (==on) (==off)
removeLrecBracketsF :: (Eq a) => (TermStruc a -> Bool) -> (TermStruc a -> Bool) -> TermStruc a -> TermStruc a
removeLrecBracketsF onf offf (SAPPL (SAPPL on' t) off')
  | onf on' && offf off' = removeLrecBracketsF onf offf t
removeLrecBracketsF onf offf (SAPPL a b) = SAPPL
                              (removeLrecBracketsF onf offf a)
                              (removeLrecBracketsF onf offf b)
removeLrecBracketsF _ _ t = t

removeRrecBrackets :: (Eq a) => TermStruc a -> TermStruc a -> TermStruc a -> TermStruc a
removeRrecBrackets on off = removeRrecBracketsF (==on) (==off)
removeRrecBracketsF :: (Eq a) => (TermStruc a -> Bool) -> (TermStruc a -> Bool) -> TermStruc a -> TermStruc a
removeRrecBracketsF onf offf (SAPPL on' (SAPPL t off'))
  | onf on' && offf off' = removeRrecBracketsF onf offf t
removeRrecBracketsF onf offf (SAPPL a b) = SAPPL
                              (removeRrecBracketsF onf offf a)
                              (removeRrecBracketsF onf offf b)
removeRrecBracketsF _ _ t = t
