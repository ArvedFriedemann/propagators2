module Control.Propagator.CellAction
    ( CellAction
    , readAction
    , propagate
    , eval
    ) where

import "base" Data.Functor

import "this" Control.Propagator.Class


data CellAction m a where
    Pure :: a -> CellAction m a
    Read :: Value a => Cell m a -> (a -> b) -> CellAction m b
    CApp :: CellAction m (a -> b) -> CellAction m a -> CellAction m b

readAction :: Value a => Cell m a -> CellAction m a
readAction = flip Read id

instance Functor (CellAction m) where
    f `fmap` Pure a = Pure (f a)
    g `fmap` Read c f = Read c (g . f)
    f `fmap` c = pure f <*> c

instance Applicative (CellAction m) where
    pure = Pure
    Pure f <*> a = f <$> a
    a <*> b = CApp a b 

propagate :: (Value a, PropagatorMonad m) => Cell m a -> CellAction m a -> m ()
propagate = eval . write

eval :: PropagatorMonad m => (a -> m ()) -> CellAction m a -> m ()
eval f (Pure a) = f a
eval f (Read s g) = void $ watch s ("" :: String) (f . g)
eval f (CApp g a) = flip eval g $ \ vg -> flip eval a (f . vg) 
