{-# LANGUAGE NoImplicitPrelude #-}
module Data.Iso
    ( module Exp
    , type(<->)
    , IsIso(..)
    , type IsoM
    , embedM, projectM
    , embedF, projectF
    , type (<-?->)
    , dual
    , swapped
    , coerce, Coerced(..)
    , enum, integral
    , equality
    , list
    , typeable
    , generic, generic1
    , liftIso
    , functor
    ) where

import "base" Prelude hiding ( (.), id )
import "base" Data.Tuple ( swap )
import "base" Data.Bifunctor
import "base" Data.Proxy
import "base" Control.Category
import "base" Control.Arrow
import "base" Data.Coerce qualified as D
import "base" Data.Typeable
import "base" GHC.Exts ( IsList(..) )
import "base" GHC.Generics ( Generic( Rep ), Generic1( Rep1 ) )
import "base" GHC.Generics qualified as Gen

import "semigroupoids" Data.Isomorphism as Exp

import "this" Data.NaturalTransformation


type (<->) = Iso (->)
infixr 1 <->
type IsoM m = Iso (Kleisli m)

class (Ord i, Typeable i, Show i) => IsIso i a b | i -> a, i -> b where
    theIso :: i -> a <-> b

instance Typeable a => IsIso (Proxy a) a a where
    theIso _ = id    

instance (IsIso i0 a0 b0, IsIso i1 a1 b1)  => IsIso (i0, i1) (a0, a1) (b0, b1) where
    theIso (theIso -> (Iso ei0 pi0), theIso -> (Iso ei1 pi1)) = bimap ei0 ei1 `Iso` bimap pi0 pi1

embedM :: IsoM m a b -> a -> m b
embedM = runKleisli . embed

projectM :: IsoM m a b -> b -> m a
projectM = runKleisli . project

type (<-?->) = IsoM Maybe
infixr 1 <-?->

dual :: Iso cat a b -> Iso cat b a
dual (Iso t f) = Iso f t

swapped :: (a, b) <-> (b, a)
swapped = Iso swap swap

coerce :: (D.Coercible a b, D.Coercible b a) => a <-> b
coerce = Iso D.coerce D.coerce

data Coerced a b = Coerced deriving (Eq, Ord, Show)
instance (Typeable a, Typeable b, D.Coercible a b, D.Coercible b a) => IsIso (Coerced a b) a b where
    theIso _ = coerce

enum :: Enum a => Int <-> a
enum = Iso toEnum fromEnum

integral :: (Integral a, Integral b) => a <-> b
integral = Iso (fromInteger . toInteger) (fromInteger . toInteger)

equality :: (a :~: b) -> a <-> b
equality Refl = id

instance (Typeable a, Typeable b) => IsIso (a :~: b) a b where
    theIso Refl = id

list :: IsList l => l <-> [Item l]
list = toList `Iso` fromList

typeable :: (Typeable a, Typeable b) => a <-?-> b
typeable = Kleisli cast `Iso` Kleisli cast

generic :: Generic a => a <-> Rep a x
generic = Gen.from `Iso` Gen.to

liftIso :: Category cat => Iso cat a b -> cat a a -> cat b b
liftIso i f = embed i . f . project i

type (<~>) = Iso (:~>)
infixr 1 <~>

embedF :: f <~> g -> f a -> g a
embedF = nt . embed

projectF :: f <~> g -> g a -> f a
projectF = nt . project

functor :: (Functor f, Functor g)
     => f <~> g -> a <-> b -> f a <-> g b
functor n i = Iso (embedF n . fmap (embed i)) (projectF n . fmap (project i))

generic1 :: Generic1 f => f <~> Rep1 f
generic1 = NT Gen.from1 `Iso` NT Gen.to1
