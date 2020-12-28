module Control.Propagator.CellAction
    ( CellAction(Read, Watch)
    , propagate
    , eval
    ) where

import "base" Data.Functor

import "this" Control.Propagator.Class


data CellAction m a where
    Pure :: a -> CellAction m a
    Read :: Value a => Cell m a -> CellAction m a
    Watch :: Value a => Cell m a -> CellAction m a
    CApp :: CellAction m (a -> b) -> CellAction m a -> CellAction m b

instance Functor (CellAction m) where
    f `fmap` Pure a = pure $ f a
    f `fmap` c = pure f <*> c

instance Applicative (CellAction m) where
    pure = Pure
    Pure f <*> a = f <$> a
    a <*> b = CApp a b

propagate :: (Value a, Monad m, PropagatorMonad m)
          => Cell m a -> CellAction m a -> m ()
propagate = eval . write

eval :: (Monad m, PropagatorMonad m) => (a -> m ()) -> CellAction m a -> m ()
eval f (Pure a) = f a
eval f (Read s) = readCell s >>= f
eval f (Watch s) = void $ watch s f
eval f (CApp g a) = flip eval g $ \ vg -> flip eval a (f . vg)
