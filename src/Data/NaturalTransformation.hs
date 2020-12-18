{-# LANGUAGE NoImplicitPrelude #-}
module Data.NaturalTransformation where

import "base" Prelude hiding ( (.), id )
import "base" Control.Category


type (~>) f g = forall a. f a -> g a
infixr 1 ~>

newtype (:~>) f g = NT
    { nt :: forall a. f a -> g a
    }
instance Category (:~>) where
    id = NT id
    NT f . NT g = NT $ f . g

class Unconstrained a
instance Unconstrained a

class FFunctor c ff where
    ntMap :: (Functor g, Functor h) => proxy c -> (forall a. c a => g a -> h a) -> ff g -> ff h

instance FFunctor Unconstrained ((:~>) f) where
    ntMap _ f g = NT f . g

newtype Struct a f = Struct
    { getStruct :: f a
    }

instance c a => FFunctor c (Struct a) where
    ntMap _ f = Struct . f . getStruct


newtype Fix f = Fix
    { unFix :: f (Fix f)
    }

instance FFunctor Unconstrained Fix where
    ntMap pc f = Fix . fmap (ntMap pc f) . f . unFix

class FTraversable c ff where
    traverseF :: (Functor f, Applicative g, Functor h) 
              => proxy c
              -> (forall a. c a => f a -> g (h a)) -> ff f -> g (ff h)

instance c a => FTraversable c (Struct a) where
    traverseF _ f (Struct a) = Struct <$> f a
