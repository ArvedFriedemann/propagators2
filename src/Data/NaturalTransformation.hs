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

class FFunctor ff where
    ntMap :: (Functor g, Functor h) => g ~> h -> ff g -> ff h
    ntMap = (<$$>)
    (<$$>) :: (Functor g, Functor h) => g ~> h -> ff g -> ff h
    (<$$>) = ntMap
    {-# MINIMAL ( ntMap | (<$$>) ) #-}

(<$$) :: ( FFunctor ff
         , Functor g, Functor h
         )
      => (forall a. h a) -> ff g -> ff h
(<$$) f = ntMap $  \ _ -> f

instance FFunctor ((:~>) f) where
    ntMap f g = NT f . g

newtype Struct a f = Struct
    { getStruct :: f a
    }

instance FFunctor (Struct a) where
    ntMap f = Struct . f . getStruct


newtype Fix f = Fix
    { unFix :: f (Fix f)
    }

instance FFunctor Fix where
    ntMap f = Fix . fmap (ntMap f) . f . unFix
