{-# LANGUAGE UndecidableInstances #-}
module Data.HList.HList where

import "base" Data.Functor.Identity

import "this" Data.IsTuple

import "this" Data.HList.HListT


newtype HList l = MkHList
    { getHLT :: HListT l Identity
    }
{-# COMPLETE HNil :: HList #-}
{-# COMPLETE HCons :: HList #-}

{-# COMPLETE HList :: HList #-}
pattern HList :: (IsTuple (HList l), l ~ (Components (HList l))) => Tuple l -> HList l
pattern HList a = AsTuple a

pattern HNil :: HList '[]
pattern HNil = MkHList HNilT


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
