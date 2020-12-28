{-# LANGUAGE DefaultSignatures #-}
module Data.ShowM where

import "base" Data.Functor.Identity


class ShowM m a where
    showM :: a -> m String
    default showM :: Functor m => a -> m String
    showM = fmap ($ "") . showsPrecM 0 
    
    showsPrecM :: Int -> a -> m ShowS
    default showsPrecM :: Functor m => Int -> a -> m ShowS
    showsPrecM _ = fmap const . showM
    {-# MINIMAL showM | showsPrecM #-}

instance (Applicative m, Show a) => ShowM m (Identity a) where
    showM = pure . show . runIdentity
    showsPrecM i = pure . showsPrec i . runIdentity