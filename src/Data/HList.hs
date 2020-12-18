{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude    #-}
module Data.HList where

import "base" Prelude hiding ( concat )
import "base" Data.Proxy
import "base" Data.Functor.Identity
import "base" Data.Kind
import "base" Data.Type.Equality
import "base" GHC.TypeLits

import "this" Data.IsTuple
import "this" Data.NaturalTransformation


type family (++) as bs where
    (++) '[] bs = bs
    (++) (a ': as) bs = a ': (as ++ bs)

data HListT (l :: [Type]) f where
    HNilT  :: HListT '[] f
    HConsT :: f a -> HListT as f -> HListT (a ': as) f
infixr 5 `HConsT`

instance Eq (HListT '[] f) where
    _ == _ = True
instance (Eq (f a), Eq (HListT as f)) => Eq (HListT (a ': as) f) where
    HConsT a as == HConsT b bs = a == b && as == bs

instance Ord (HListT '[] f) where
    _ `compare` _ = EQ
instance (Ord (f a), Ord (HListT as f)) => Ord (HListT (a ': as) f) where
    HConsT a as `compare` HConsT b bs = a `compare` b <> as `compare` bs

instance ShowHListT f l => Show (HListT l f) where
    showsPrec d l 
        = showParen (d >= 10)
        $ showString "HListT ["
        . (drop 2 . showsHListT l)
        . showString "]"
class ShowHListT f l where
    showsHListT :: HListT l f -> ShowS
instance ShowHListT f '[] where
    showsHListT _ = id
instance (Show (f a), ShowHListT f as) => ShowHListT f (a ': as) where
    showsHListT (HConsT a as)
        = showString ", "
        . shows a
        . showsHListT as

instance FFunctor c (HListT '[]) where
    ntMap _ _ _ = HNilT
instance (c a, FFunctor c (HListT as)) => FFunctor c (HListT (a ': as)) where
    ntMap pc f (HConsT a as) = HConsT (f a) $ ntMap pc f as

instance FTraversable c (HListT '[]) where
    traverseF _ _ _ = pure HNilT
instance (c a, FTraversable c (HListT as)) => FTraversable c (HListT (a ': as)) where
    traverseF pc f (HConsT a as) = HConsT <$> f a <*> traverseF pc f as

{-# COMPLETE HListT :: HListT #-}
pattern HListT :: IsTuple (HListT l f) => Tuple (Components (HListT l f)) -> HListT l f
pattern HListT a = AsTuple a

newtype HList l = MkHList
    { getHLT :: HListT l Identity
    }
{-# COMPLETE HNil :: HList #-}
{-# COMPLETE HCons :: HList #-}


instance Eq (HList '[]) where
    _ == _ = True
instance (Eq a, Eq (HList as)) => Eq (HList (a ': as)) where
    HCons a as == HCons b bs = a == b && as == bs

instance Ord (HList '[]) where
    _ `compare` _ = EQ
instance (Ord a, Ord (HList as)) => Ord (HList (a ': as)) where
    HCons a as `compare` HCons b bs = a `compare` b <> as `compare` bs

instance ShowHList l => Show (HList l) where
    showsPrec d l 
        = showParen (d >= 10)
        $ showString "HListT ["
        . (drop 2 . showsHList l)
        . showString "]"
class ShowHList l where
    showsHList :: HList l -> ShowS
instance ShowHList '[] where
    showsHList _ = id
instance (Show a, ShowHList as) => ShowHList (a ': as) where
    showsHList (HCons a as)
        = showString ", "
        . shows a
        . showsHList as

{-# COMPLETE HList :: HList #-}
pattern HList :: (IsTuple (HList l), l ~ (Components (HList l))) => Tuple l -> HList l
pattern HList a = AsTuple a

pattern HNil :: HList '[]
pattern HNil = MkHList HNilT

uncons :: HList (a ': as) -> (a, HList as)
uncons (MkHList (HConsT (Identity a) as)) = (a, MkHList as)

pattern HCons :: a -> HList as -> HList (a ': as)
pattern HCons a as <- (uncons -> (a, as))
  where HCons a (MkHList as) = MkHList $ HConsT (Identity a) as
infixr 5 `HCons`

type family RunIdentity a where
    RunIdentity (Identity a) = a
instance HasComponent (HListT l Identity) i => HasComponent (HList l) i where
    type Component (HList l) i = RunIdentity (Component (HListT l Identity) i)
    getAt p = runIdentity . getAt p . getHLT

instance Elem' i l (i == 0) => HasComponent (HListT l f) i where
    type Component (HListT l f) i = f (ElemT' i l (i == 0))
    getAt = getT' $ Proxy @(i == 0)

class Elem' (i :: Nat) (l :: [Type]) (isZero :: Bool) where
    type ElemT' i l isZero
    getT' :: proxy0 isZero -> proxy1 i -> HListT l f -> f (ElemT' i l isZero)

instance Elem' 0 (a ': as) 'True where
    type ElemT' 0 (a ': as) 'True = a
    getT' _ _ (a `HConsT` _) = a

instance Elem' (i - 1) as ((i - 1) == 0) => Elem' i (a ': as) 'False where
    type ElemT' i (a ': as) 'False = ElemT' (i - 1) as ((i - 1) == 0)
    getT' _ _ (_ `HConsT` as) = getT' (Proxy @((i - 1) == 0)) (Proxy @(i - 1)) as

type family AllT (c :: Type -> Constraint) (f :: Type -> Type) (l :: [Type]) :: Constraint where
    AllT c f '[] = ()
    AllT c f (a ': as) = (c (f a), AllT c f as)
  
type family All (c :: Type -> Constraint) (l :: [Type]) :: Constraint where
    All c '[] = ()
    All c (a ': as) = (c a, All c as)

instance Semigroup (HListT '[] f) where
    HNilT <> HNilT = HNilT
instance (Semigroup (HListT as f), Semigroup (f a)) => Semigroup (HListT (a ': as) f) where
    (a `HConsT` as) <> (b `HConsT` bs) = (a <> b) `HConsT` (as <> bs)

instance Monoid (HListT '[] f) where
    mempty = HNilT
instance (Monoid (HListT as f), Monoid (f a)) => Monoid (HListT (a ': as) f) where
    mempty = mempty `HConsT` mempty


concat :: HListT a f -> HListT b f -> HListT (a ++ b) f
concat HNilT bs = bs
concat (a `HConsT` as) bs = a `HConsT` concat as bs


instance IsTuple (HListT '[] f) where
    type Components (HListT '[] f) = '[]
    fromTuple _ = HNilT
    asTuple _ = ()
instance ( MkTuple (f a) (Components (HListT ax f))
         , HasComponents (HListT (a ': ax) f) (Components (HListT (a ': ax) f))
         , IsTuple (HListT ax f)
         ) => IsTuple (HListT (a ': ax) f) where
    type Components (HListT (a ': ax) f) = f a ': Components (HListT ax f)
    fromTuple (ConsTuple a as) = HConsT a $ fromTuple as
    asTuple (HConsT a as) = ConsTuple a $ asTuple as

instance IsTuple (HList '[]) where
    type Components (HList '[]) = '[]
    fromTuple _ = HNil
    asTuple _ = ()
instance ( MkTuple a (Components (HList ax))
         , HasComponents (HList (a ': ax)) (Components (HList (a ': ax)))
         , IsTuple (HList ax)
         ) => IsTuple (HList (a ': ax)) where
    type Components (HList (a ': ax)) = a ': Components (HList ax)
    fromTuple (ConsTuple a as) = HCons a $ fromTuple as
    asTuple (HCons a as) = ConsTuple a $ asTuple as
