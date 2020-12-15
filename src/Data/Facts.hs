{-# LANGUAGE UndecidableInstances #-}
module Data.Facts
    ( Contradictable(..)
    , Unifiable(..)
    , Facts
    , introduce
    , ValExpr(..)
    , OrdFact(..)
    ) where

import "base" GHC.Exts ( IsList(..) )

import "this" Data.Lattice


data Contradictable a
    = Contradiction
    | Valid a
  deriving stock (Eq, Ord, Show, Read, Functor)

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


data ValExpr v a
    = RefExpr v
    | ValExpr a
  deriving stock (Eq, Ord)

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
