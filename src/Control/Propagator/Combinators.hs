{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Combinators where

import "base" Prelude hiding ( (.), id, read )
import "base" Data.Foldable
import "base" Data.Functor
import "base" Control.Category

import "this" Data.Iso
import "this" Data.Lattice
import "this" Control.Propagator.Class


iso :: (MonadProp m, Identifier i a, Identifier j b, Std p) => i -> j -> p -> (a <-> b) -> m ()
iso a b p i = do
      link a b p (embed i)
      link b a p (project i)

eq :: (MonadProp m, Identifier i a, Identifier j a) => i -> j -> m ()
eq a b = iso a b () id

eqAll :: (MonadProp m, Identifier i a) => [i] -> m ()
eqAll [] = pure mempty
eqAll (a : as) = fold <$> traverse (eq a) as

linkM :: (MonadProp m, Identifier i a, Identifier j b, Std l)
      => i -> j -> l -> (a -> m b) -> m ()
linkM ca cb l f = void . watch ca l $ \ a -> f a >>= write cb

linkM2 :: (BoundedMeet a, BoundedMeet b, MonadProp m, Identifier i a, Identifier j b, Identifier k c, Std l)
       => i -> j -> k -> l -> (a -> b -> m c) -> m ()
linkM2 ca cb cc l f = void $ watch ca l (watchM2 cc cb . f) *> watch cb l (watchM2 cc ca . flip f)
  where
    watchM2 cc' cb' f' = read cb' >>= f' >>= write cc'

link :: (MonadProp m, Identifier i a, Identifier j b, Std p)
     => i -> j -> p -> (a -> b) -> m ()
link a b p f = void . watch a (p, b) $ write b . f

link2 :: (BoundedMeet a, BoundedMeet b, MonadProp m, Identifier i a, Identifier j b, Identifier k c, Std l)
      => i -> j -> k -> l -> (a -> b -> c) -> m ()
link2 ca cb cc i f = linkM2 ca cb cc i (\ a b -> pure $ f a b)
