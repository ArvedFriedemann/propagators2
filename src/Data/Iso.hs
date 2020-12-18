{-# LANGUAGE NoImplicitPrelude #-}
module Data.Iso where

import "base" Prelude hiding ( (.), id )
import "base" Data.Tuple ( swap )
import "base" Control.Category
import "base" Control.Arrow
import "base" Data.Coerce
import "base" Data.Typeable
import "base" GHC.Exts
import "base" GHC.Generics ( Generic( Rep ), Generic1( Rep1 ) )
import "base" GHC.Generics qualified as Gen

import "this" Data.NaturalTransformation


data Iso cat a b = (:<->)
    { to :: cat a b
    , from :: cat b a
    }
infix 0 :<->

type (<->) = Iso (->)
infixr 1 <->
type IsoM m = Iso (Kleisli m)

toM :: IsoM m a b -> a -> m b
toM = runKleisli . to

fromM :: IsoM m a b -> b -> m a
fromM = runKleisli . from

type (<-?->) = IsoM Maybe
infixr 1 <-?->

instance Category cat => Category (Iso cat) where
    id = id :<-> id
    (t :<-> f) . (t' :<-> f') = (t . t') :<-> (f' . f)

co :: Iso cat a b -> Iso cat b a
co (t :<-> f) = f :<-> t

swapped :: (a, b) <-> (b, a)
swapped = swap :<-> swap

coerceIso :: (Coercible a b, Coercible b a) => a <-> b
coerceIso = coerce :<-> coerce

enum :: Enum a => Int <-> a
enum = toEnum :<-> fromEnum

integral :: (Integral a, Integral b) => a <-> b
integral = (fromInteger . toInteger) :<-> (fromInteger . toInteger)

equality :: (a :~: b) -> a <-> b
equality Refl = id

list :: IsList l => l <-> [Item l]
list = toList :<-> fromList

typeable :: (Typeable a, Typeable b) => a <-?-> b
typeable = Kleisli cast :<-> Kleisli cast

generic :: Generic a => a <-> Rep a x
generic = Gen.from :<-> Gen.to

liftIso :: Category cat => Iso cat a b -> cat a a -> cat b b
liftIso i f = to i . f . from i

type (<~>) = Iso (:~>)
infixr 1 <~>

toF :: f <~> g -> f a -> g a
toF = nt . to

fromF :: f <~> g -> g a -> f a
fromF = nt . from

functor :: (Functor f, Functor g)
     => f <~> g -> a <-> b -> f a <-> g b
functor n i = toF n . fmap (to i) :<-> fromF n . fmap (from i)

generic1 :: Generic1 f => f <~> Rep1 f
generic1 = NT Gen.from1 :<-> NT Gen.to1
