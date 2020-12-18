{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude    #-}
module Data.HList where

import "base" Prelude ( error )
import "base" Data.Proxy
import "base" Data.Bool
import "base" Data.Functor
import "base" Data.Function ( ($), (.) )
import "base" Data.Semigroup ( Semigroup(..) )
import "base" Data.Monoid ( Monoid(..) )
import "base" Control.Applicative
import "base" Data.Functor.Identity
import "base" Data.Kind
import "base" Data.Type.Equality
import "base" GHC.TypeLits

import "this" Data.IsTuple


type family (++) as bs where
    (++) '[] bs = bs
    (++) (a ': as) bs = a ': (as ++ bs)

data HListT f (l :: [Type]) where
    HNilT  :: HListT f '[]
    HConsT :: f a -> HListT f as -> HListT f (a ': as)
infixr 5 `HConsT`

{-# COMPLETE HListT :: HListT #-}
pattern HListT :: IsTuple (HListT f l) => Tuple (Components (HListT f l)) -> HListT f l
pattern HListT a = AsTuple a

newtype HList l = MkHList
    { getHLT :: HListT Identity l
    }
{-# COMPLETE HNil :: HList #-}
{-# COMPLETE HCons :: HList #-}

{-# COMPLETE HList :: HList #-}
pattern HList :: IsTuple (HList l) => Tuple (Components (HList l)) -> HList l
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
instance HasComponent (HListT Identity l) i => HasComponent (HList l) i where
    type Component (HList l) i = RunIdentity (Component (HListT Identity l) i)
    getAt p = runIdentity . getAt p . getHLT

instance Elem' i l (i == 0) => HasComponent (HListT f l) i where
    type Component (HListT f l) i = f (ElemT' i l (i == 0))
    getAt = getT' $ Proxy @(i == 0)

class Elem' (i :: Nat) (l :: [Type]) (isZero :: Bool) where
    type ElemT' i l isZero
    getT' :: proxy0 isZero -> proxy1 i -> HListT f l -> f (ElemT' i l isZero)

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

instance Semigroup (HListT f '[]) where
    HNilT <> HNilT = HNilT
instance (Semigroup (HListT f as), Semigroup (f a)) => Semigroup (HListT f (a ': as)) where
    (a `HConsT` as) <> (b `HConsT` bs) = (a <> b) `HConsT` (as <> bs)

instance Monoid (HListT f '[]) where
    mempty = HNilT
instance (Monoid (HListT f as), Monoid (f a)) => Monoid (HListT f (a ': as)) where
    mempty = mempty `HConsT` mempty


concat :: HListT f a -> HListT f b -> HListT f (a ++ b)
concat HNilT bs = bs
concat (a `HConsT` as) bs = a `HConsT` concat as bs

mapAT :: (Applicative f, All c l) => proxy c -> (forall a. c a => g a -> f (h a)) -> HListT g l -> f (HListT h l)
mapAT _ _ HNilT = pure HNilT
mapAT p f (a `HConsT` as) = liftA2 HConsT (f a) (mapAT p f as)

mapA :: forall f l c proxy. (Applicative f, All c l) => proxy c -> (forall a. c a => a -> f a) -> HList l -> f (HList l)
mapA p f = fmap MkHList . (mapAT p $ fmap Identity . f . runIdentity) . getHLT


instance IsTuple (HListT ff '[]) where
    type Components (HListT ff '[]) = '[]
    fromTuple _ = HNilT
    asTuple _ = ()
instance ( MkTuple (ff a) (Components (HListT ff ax))
         , HasComponents (HListT ff (a ': ax)) (Components (HListT ff (a ': ax)))
         , IsTuple (HListT ff ax)
         ) => IsTuple (HListT ff (a ': ax)) where
    type Components (HListT ff (a ': ax)) = ff a ': Components (HListT ff ax)
    fromTuple (ConsTuple a as) = HConsT a $ fromTuple as
    fromTuple _ = error "only present for pattern exhaustiveness-checker"
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
    fromTuple _ = error "only present for pattern exhaustiveness-checker"
    asTuple (HCons a as) = ConsTuple a $ asTuple as
