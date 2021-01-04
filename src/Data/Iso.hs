{-# LANGUAGE NoImplicitPrelude #-}
module Data.Iso
    ( module Exp
    , type(<->)
    , type IsoM
    , embedM, projectM
    , embedF, projectF
    , type (<-?->)
    , co
    , swapped, coerce
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

embedM :: IsoM m a b -> a -> m b
embedM = runKleisli . embed

projectM :: IsoM m a b -> b -> m a
projectM = runKleisli . project

type (<-?->) = IsoM Maybe
infixr 1 <-?->

co :: Iso cat a b -> Iso cat b a
co (Iso t f) = Iso f t

swapped :: (a, b) <-> (b, a)
swapped = Iso swap swap

coerce :: (D.Coercible a b, D.Coercible b a) => a <-> b
coerce = Iso D.coerce D.coerce

enum :: Enum a => Int <-> a
enum = Iso toEnum fromEnum

integral :: (Integral a, Integral b) => a <-> b
integral = Iso (fromInteger . toInteger) (fromInteger . toInteger)

equality :: (a :~: b) -> a <-> b
equality Refl = id

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
