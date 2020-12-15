{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Vec
    ( Vec
    , get
    , dot
    , reversev
    ) where

import "base" Data.Proxy
import "base" Data.Kind
import "base" Control.Applicative
import "base" GHC.TypeNats
import "base" GHC.Exts ( IsList(..) )


newtype Vec (n :: Nat) (a :: Type) = MkVec [a]
  deriving newtype (Eq, Ord, Functor, Foldable)

instance KnownNat n => Traversable (Vec n) where
    traverse f (MkVec l) = fmap (MkVec . getZipList) (traverse f (ZipList l)) 

instance KnownNat n => Applicative (Vec n) where
    pure = MkVec . take (n @n) . repeat
    liftA2 f (MkVec a) (MkVec b) = MkVec (zipWith f a b)

dot :: (KnownNat n, Num a) => Vec n a -> Vec n a -> a
dot a b = sum $ liftA2 (*) a b

n :: forall (n :: Nat). KnownNat n => Int
n = fromEnum (natVal (Proxy @n))

reversev :: Vec n a -> Vec n a
reversev (MkVec l) = MkVec $ reverse l

instance KnownNat n => IsList (Vec n a) where
    type Item (Vec n a) = a
    toList (MkVec l) = l
    fromList l = let n' = n @n; l' = length l
                  in if n' == l' then MkVec l
                  else error $ "expected list of length " ++ show n' ++ " but got " ++ show l'

get :: forall (n :: Nat) (m :: Nat) (a :: Type)
    . (KnownNat n, n <= (1 + m))
    => Vec m a -> a
get (MkVec l) = l !! (n @n)
