{-# LANGUAGE UndecidableInstances #-}
module Data.Facts
    ( Contradictable(..)
    , Unifiable(..)
    , Facts
    , introduce
    , ValExpr(..)
    , OrdFact(..)
    ) where

import "base" Control.Applicative ( liftA2 )

import "base" GHC.Exts ( IsList(..) )

import "this" Data.Lattice


data Contradictable a
    = Contradiction
    | Valid a
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Applicative Contradictable where
    pure = Valid
    Valid f <*> Valid a = Valid $ f a
    _       <*> _       = Contradiction

class Ord a => Unifiable a where
    unify :: a -> a -> Contradictable (Maybe a)

newtype Facts a = Facts (Contradictable [a])
  deriving newtype (Eq, Ord)

instance Show a => Show (Facts a) where
    showsPrec d (Facts fx)
        = showParen (d >= 10)
        $ showString "Facts "
        . showFx
      where
        showFx = case fx of
            Contradiction -> shows (Contradiction @a)
            (Valid f) -> shows f

instance Foldable Facts where
    foldMap f (Facts (Valid s)) = foldMap f s
    foldMap _ _ = mempty

instance Unifiable a => IsList (Facts a) where
    type Item (Facts a) = a
    fromList = foldr introduce mempty
    toList (Facts (Valid s)) = s
    toList _ = []

instance Unifiable a => Semigroup (Facts a) where
    Facts Contradiction <> _ = Facts Contradiction
    _ <> Facts Contradiction = Facts Contradiction
    a <> b = foldr introduce a b

instance Unifiable a => Monoid (Facts a) where
    mempty = Facts . Valid $ []

introduce :: forall a. Unifiable a => a -> Facts a -> Facts a
introduce _ (Facts Contradiction) = Facts Contradiction
introduce a (Facts (Valid bs)) = Facts $ go a bs
  where
    go :: a -> [a] -> Contradictable [a]
    go x [] = pure [x]
    go x (b : bx) = case unify x b of
      Contradiction   -> Contradiction
      Valid (Just a') -> go a' bx
      Valid Nothing   -> (b :) <$> go x bx

deriving via (Monoidal (Facts a)) instance Unifiable a => Meet (Facts a)
deriving via (Monoidal (Facts a)) instance Unifiable a => BoundedMeet (Facts a)

factsBinOp :: (Contradictable a -> Contradictable a -> Contradictable a) -> Facts a -> Facts a -> Facts a
factsBinOp f (Facts (Valid as)) (Facts (Valid bs)) = Facts . Valid . concat $ liftA2 f' as bs
  where f' a b = case f (Valid a) (Valid b) of
          Contradiction -> [a, b]
          Valid c -> [c]
factsBinOp _ _ _ = Facts Contradiction


factsOp :: (Contradictable a -> Contradictable a) -> Facts a -> Facts a
factsOp f (Facts (Valid as)) = Facts . Valid . fmap f' $ as
  where f' a = case f (Valid a) of
          Contradiction -> a
          Valid c -> c
factsOp _ _ = Facts Contradiction

instance Num (Contradictable a) => Num (Facts a) where
    fromInteger = Facts . fmap pure . fromInteger
    (+) = factsBinOp (+)
    (*) = factsBinOp (*)
    (-) = factsBinOp (-)
    abs = factsOp abs
    negate = factsOp negate
    signum = factsOp signum


data ValExpr v a
    = RefExpr v
    | ValExpr a
  deriving stock (Eq, Ord)


valExprBinOp :: (a -> a -> a) -> ValExpr v a -> ValExpr v a -> ValExpr v a
valExprBinOp f (ValExpr a) (ValExpr b) = ValExpr $ f a b
valExprBinOp _ _ _ = error "can not perform bin op on RefExpr"

valExprOp :: (a -> a) -> ValExpr v a -> ValExpr v a
valExprOp f (ValExpr a) = ValExpr . f $ a
valExprOp _ _ = error "can not perform op on RefExpr"

instance Num a => Num (ValExpr v a) where
    fromInteger = ValExpr . fromInteger
    (+) = valExprBinOp (+)
    (*) = valExprBinOp (*)
    (-) = valExprBinOp (-)
    negate = valExprOp negate
    abs = valExprOp abs
    signum = valExprOp signum

instance (Show v, Show a) => Show (ValExpr v a) where
    show (RefExpr v) = '#' : show v
    show (ValExpr a) = show a
                        
data OrdFact v a = OrdFact Ordering (ValExpr v a)
  deriving stock (Eq, Ord)

instance (Show v, Show a) => Show (OrdFact v a) where
    showsPrec d (OrdFact r a)
        = showParen (d >= 10)
        $ shows r
        . showString " "
        . shows a

instance (Ord v, Ord a) => Unifiable (OrdFact v a) where
    unify fa@(OrdFact r (ValExpr a)) fb@(OrdFact r' (ValExpr b)) = case (r, r') of
        (EQ, EQ) | a == b -> uni fa
        (EQ, LT) | a < b  -> uni fa
        (EQ, GT) | a > b  -> uni fa
        (LT, EQ) | b < a  -> uni fb
        (GT, EQ) | b > a  -> uni fb
        (LT, LT) -> uni . (OrdFact LT) . ValExpr $ max a b
        (GT, GT) -> uni . (OrdFact GT) . ValExpr $ min a b
        _ -> Contradiction
      where uni = pure . pure
    unify _ _ = pure Nothing

ordFactBinOp :: (a -> a -> a) -> Contradictable (OrdFact v a) -> Contradictable (OrdFact v a) -> Contradictable (OrdFact v a)
ordFactBinOp f (Valid (OrdFact r (ValExpr a))) (Valid (OrdFact r' (ValExpr b)))
    | r == r' = Valid . OrdFact r . ValExpr $ f a b
ordFactBinOp _ _ _ = Contradiction


ordFactOp :: (a -> a) -> Contradictable (OrdFact v a) -> Contradictable (OrdFact v a)
ordFactOp f (Valid (OrdFact r (ValExpr a))) = Valid . OrdFact r . ValExpr $ f a
ordFactOp _ _ = Contradiction


instance (Ord v, Ord a, Num a) => Num (Contradictable (OrdFact v a)) where
  fromInteger = Valid . OrdFact EQ . ValExpr . fromInteger
  (+) = ordFactBinOp (+)
  (*) = ordFactBinOp (*)
  (-) = ordFactBinOp (-)
  abs = ordFactOp abs
  signum = ordFactOp signum
  negate = ordFactOp negate
  