{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Combinators where

import "base" Prelude hiding ( (.), id, read )
import "base" Data.Functor
import "base" Data.Typeable
import "base" Control.Category

import "this" Data.Iso
import "this" Control.Propagator.Class
import "this" Control.Propagator.Base
import "this" Control.Propagator.Propagator
import "this" Control.Propagator.Scope


--TODO: No idea whether I did this right
recursiveCall :: (MonadProp m, Identifier w a) => w -> m () -> m ()
recursiveCall = undefined -- TODO


data IsoTo i b = IsoTo i b deriving (Eq, Ord, Show)
instance (MonadProp m, Value a, Propagator m i b, IsIso j a b) => Propagator m (IsoTo j i) a where
    propagate (IsoTo i b) = propagate b . embed (theIso i)

data IsoFrom i a = IsoFrom i a deriving (Eq, Ord, Show)
instance (MonadProp m, Value b, Propagator m i a, IsIso j a b) => Propagator m (IsoFrom j i) b where
    propagate (IsoFrom i a) = propagate a . project (theIso i)

iso :: (MonadProp m, Value a, Value b, Identifier i a, Identifier j b, IsIso p a b) => p -> i -> j -> m ()
iso i a b = void $ do
    watch a $ IsoTo i $ Write b
    watch b $ IsoFrom i $ Write a

eq :: forall a i j m. (MonadProp m, Value a, Identifier i a, Identifier j a) => i -> j -> m ()
eq = iso $ Proxy @a

eqAll :: (MonadProp m, Foldable t, Value a, Identifier i a) => t i -> m ()
eqAll t = maybe (pure ()) void $ foldr eqAll' Nothing t
  where
    eqAll' i Nothing  = Just (pure i)
    eqAll' i (Just j) = Just (j >>= eq i >> pure i)

data SetEq i = SetEq i deriving (Eq, Ord, Show)
instance (MonadProp m, Value a, Identifier i a, Identifier j a) => Propagator m (SetEq i) j where 
    propagate (SetEq i) = eq i

newtype Write i = Write i deriving (Eq, Ord, Show)
instance (MonadProp m, Value a, Identifier i a) => Propagator m (Write i) a where
    propagate (Write i) = void . write i

data Scoped i = Scoped Scope i deriving (Eq, Ord, Show)
instance (MonadProp m, Propagator m i a) => Propagator m (Scoped i) a where
    propagate (Scoped s i) = inScope s . propagate i


push :: (MonadProp m, Value a, Identifier i a, Identifier j a) => Scope -> i -> j -> m i
push s i = watch i . Scoped s . Write

promote :: (MonadProp m, Value a, Identifier t a) => Scope -> t -> m t
promote s i = push s i i

scoped :: (MonadProp m, Std i) => i -> (Scope -> m a) -> m a
scoped i f = do
    s <- scope
    inScope (pushScope i s) $ f s

data Const i a = Const a i deriving (Eq, Ord, Show)
instance Propagator m i a => Propagator m (Const i a) a where
    propagate (Const a i) _ = propagate i a

newtype Fst i = Fst i deriving (Eq, Ord, Show)
instance (Value b, Propagator m i a) => Propagator m (Fst i) (a, b) where
    propagate (Fst i) (a, _) = propagate i a
instance (Value b, Value c, Propagator m i a) => Propagator m (Fst i) (a, b, c) where
    propagate (Fst i) (a, _, _) = propagate i a

newtype Snd i = Snd i deriving (Eq, Ord, Show)
instance (Value a, Propagator m i b) => Propagator m (Snd i) (a, b) where
    propagate (Snd i) (_, b) = propagate i b
instance (Value a, Value c, Propagator m i b) => Propagator m (Snd i) (a, b, c) where
    propagate (Snd i) (_, b, _) = propagate i b
    