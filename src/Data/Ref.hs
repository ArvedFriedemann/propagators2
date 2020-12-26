{-# LANGUAGE StaticPointers       #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Ref
    ( Refable
    , Ref
    , liftRef
    , defere
    , deRef
    , staticId
    , staticCompose
    , (.#), ($#), ($##)
    , FunRef
    , funRef
    , toRef
    , Dict(..)
    ) where

import "base" Prelude hiding ( (.), id )
import "base" Control.Category
import "base" Data.Monoid
import "base" Data.List
import "base" Data.Function ( on )
import "base" Data.Proxy
import "base" Data.Functor.Classes
import "base" Data.Typeable
import "base" GHC.StaticPtr

import "constraints" Data.Constraint


class (Typeable a, Eq a, Show a) => Refable a
instance (Typeable a, Eq a, Show a) => Refable a


data Ref a where
    Defered :: (Typeable c, c) => Ref (Dict c -> a) -> Ref a
    Val :: Refable a => a -> Ref a
    Ref :: StaticPtr a -> Ref a
    App :: Ref (a -> b) -> Ref a -> Ref b

instance IsStatic Ref where
    fromStaticPtr = Ref

instance Eq (Ref a) where
    (==) = liftEq undefined

instance Eq1 Ref where
    liftEq _ (Val a) (Val b) = cast a == Just b
    liftEq _ (Ref a) (Ref b) = staticKey a == staticKey b
    liftEq _ (App f a) (App g b) = liftEq undefined f g && liftEq undefined a b
    liftEq _ (Defered a) (Defered b) = liftEq undefined a b
    liftEq _ _ _ = False

instance Ord (Ref a) where
    compare = liftCompare undefined

instance Ord1 Ref where
    liftCompare _ (Defered a) (Defered b) = liftCompare undefined a b
    liftCompare _ (Val a) (Val b) = on compare show (cast a) (Just b)
    liftCompare _ (Ref a) (Ref b) = staticKey a `compare` staticKey b
    liftCompare _ (App f a) (App g b) = liftCompare undefined f g <> liftCompare undefined a b
    liftCompare _ (Defered _) _ = GT
    liftCompare _ _ (Defered _) = LT
    liftCompare _ (Val _) _ = GT
    liftCompare _ _ (Val _) = LT
    liftCompare _ (Ref _) _ = GT
    liftCompare _ _ (Ref _) = LT

instance Show (Ref a) where
    showsPrec d (Val a) = showsPrec d a
    showsPrec d (Defered a)
        = showParen (d >= 10)
        $ showsPrec 10 (c a)
        . showString " => "
        . shows a
      where
        c :: forall c. Typeable c => Ref (Dict c -> a) -> TypeRep
        c _ = typeRep (Proxy @c)
    showsPrec d (App f a)
        = showParen (d >= 10)
        $ shows f
        . showString " "
        . showsPrec 10 a
    showsPrec _ (Ref  ptr)
        = appEndo
        . foldMap (Endo . showString)
        . intersperse ":"
        . ptrInfoParts
        . staticPtrInfo
        $ ptr
      where
        ptrInfoParts (StaticPtrInfo pkg mdl (line, col))
            = [pkg, mdl, show line, show col]  

liftRef :: Refable a => a -> Ref a
liftRef = Val

deRef :: Ref a -> a
deRef (Ref c) = deRefStaticPtr c
deRef (Val a) = a
deRef (App f a) = deRef f $ deRef a
deRef (Defered f) = deRef (f $# Val Dict)

defere :: forall c a. (Typeable c, c) => Ref (Dict c -> a) -> Ref a
defere = Defered

($#) :: Ref (a -> b) -> Ref a -> Ref b
($#) = App
infixl 0 $#

staticId :: forall k (cat :: k -> k -> *) a. 
          (Typeable k, Typeable cat, Category cat, Typeable a)
         => Ref (cat a a)
staticId = defere @(Category cat) $ static \Dict -> id

staticCompose :: forall k (cat :: k -> k -> *) a b c.
               ( Typeable k, Typeable cat, Category cat, Typeable a, Typeable b, Typeable c)
              => Ref (cat b c -> cat a b -> cat a c)
staticCompose = defere @(Category cat) $ static \Dict -> (.)

(.#) :: (Typeable a, Typeable b, Typeable c)
     => Ref (b -> c) -> Ref (a -> b) -> Ref (a -> c)
f .# g = staticCompose $# f $# g
infixr 9 .# 

($##) :: FunRef a b -> Ref a -> Ref b
FRId $## a = a
FunRef f $## a = f $# a
infixl 0 $##

data FunRef a b where
    FRId :: FunRef a a
    FunRef :: (Typeable a, Typeable b) => Ref (a -> b) -> FunRef  a b

funRef :: (Typeable a, Typeable b) => Ref (a -> b) -> FunRef  a b
funRef = FunRef

instance Category FunRef where
    id = FRId
    FRId . f = f
    f . FRId = f
    FunRef f . FunRef g = FunRef (f .# g)

toRef :: (Typeable a, Typeable b) => FunRef a b -> Ref (a -> b)
toRef FRId = staticId
toRef (FunRef f) = f
