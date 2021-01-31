{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Combinators where

import "base" Prelude hiding ( (.), id, read )
import "base" Data.Functor
import "base" Data.Typeable
import "base" Control.Category
import "base" Control.Monad
import "base" Debug.Trace

import "this" Data.Iso
import "this" Data.Some
import "this" Data.Lattice
import "this" Control.Propagator.Class
import "this" Control.Propagator.Base
import "this" Control.Propagator.Propagator
import "this" Control.Propagator.Scope


--TODO: No idea whether I did this right
--recursiveCall :: (MonadProp m, Identifier w a) => w -> m () -> m ()
--recursiveCall = undefined -- TODO


data IsoTo i b = IsoTo i b deriving (Eq, Ord, Show)
instance (MonadProp m, Value a, Propagator m b i, IsIso j a b) => Propagator m a (IsoTo j i) where
    propagate (IsoTo i b) = propagate b . embed (theIso i)

data IsoFrom i a = IsoFrom i a deriving (Eq, Ord, Show)
instance (MonadProp m, Value b, Propagator m a i, IsIso j a b) => Propagator m b (IsoFrom j i) where
    propagate (IsoFrom i a) = propagate a . project (theIso i)

iso :: (MonadProp m, Identifier i a, Identifier j b, IsIso p a b) => p -> i -> j -> m ()
iso i a b = void $ do
    watch a $ IsoTo i $ Write b
    watch b $ IsoFrom i $ Write a

eq :: forall a i j m. (MonadProp m, Identifier i a, Identifier j a) => i -> j -> m ()
eq = iso $ Proxy @a

eqAll :: (MonadProp m, Foldable t, Identifier i a) => t i -> m ()
eqAll t = maybe (pure ()) void $ foldr eqAll' Nothing t
  where
    eqAll' i Nothing  = Just (pure i)
    eqAll' i (Just j) = Just (j >>= eq i >> pure i)


data PropBot i = PropBot i deriving (Eq, Ord, Show)
instance (MonadProp m, Value b, Identifier i b, BoundedJoin b, Value a, BoundedJoin a) => Propagator m a (PropBot i) where
    propagate (PropBot i) a = when (isBot a) . void $ write i Bot

propBot :: (MonadProp m, Value b, Identifier i b, BoundedJoin b, Value a, Identifier j a, BoundedJoin a) => j -> i -> m ()
propBot orig targ = void $ watch orig $ PropBot targ



data SetEq i = SetEq i deriving (Eq, Ord, Show)
instance (MonadProp m, Identifier i a, Identifier j a) => Propagator m j (SetEq i) where
    propagate (SetEq i) = eq i

newtype Write i = Write i deriving (Eq, Ord, Show)
instance (MonadProp m, Value a, Identifier i a) => Propagator m a (Write i) where
    propagate (Write i) = void . write i


data ParScoped i = ParScoped i deriving (Eq, Ord, Show)
instance (MonadProp m, Propagator m a i) => Propagator m a (ParScoped i) where
    propagate (ParScoped i) = liftParent . propagate i

data Forked j i = Forked j i deriving (Eq, Ord, Show)
instance (MonadProp m, Std j, Propagator m a i) => Propagator m a (Forked j i) where
    propagate (Forked j i) a = scoped j (const $ propagate i a)


push :: (MonadProp m, Value a, Identifier i a, Identifier j a) => i -> j -> m ()
push i j = void $ watch i (ParScoped $ Write j)

pull :: (MonadProp m, Value a, Identifier i a, Identifier j a) => i -> j -> m ()
pull i j = do
  s <- scope
  case s of
    (_ :/ n) -> void $ liftParent $ do
      watch i $ Forked n (Write j)
    _ -> pure ()


promote :: (MonadProp m, Value a, Identifier t a) => t -> m ()
promote i = push i i

request :: (MonadProp m, Value a, Identifier t a) => t -> m ()
request i = pull i i

{-
push :: (MonadProp m, Value a, Identifier i a, Identifier j a) => Scope -> i -> j -> m ()
push s i j = void . watch i . Scoped s . Write $ j

promote :: (MonadProp m, Value a, Identifier t a) => Scope -> t -> m ()
promote s i = push s i i
-}

scoped :: (MonadProp m, Std i) => i -> (Scope -> m a) -> m a
scoped i f = do
    s <- scope
    inScope (s :/ i) $ f s


watchFixpoint :: (MonadProp m, Typeable m, Std k) => k -> m () -> m ()
watchFixpoint name act = void $ watch Fixpoint (UniversalNamedPropagator name act)


data Const i a = Const a i deriving (Eq, Ord, Show)
instance Propagator m a i => Propagator m a (Const i a) where
    propagate (Const a i) _ = propagate i a

newtype Fst i = Fst i deriving (Eq, Ord, Show)
instance (Value b, Propagator m a i) => Propagator m (a, b) (Fst i) where
    propagate (Fst i) (a, _) = propagate i a
instance (Value b, Value c, Propagator m a i) => Propagator m (a, b, c) (Fst i) where
    propagate (Fst i) (a, _, _) = propagate i a

newtype Snd i = Snd i deriving (Eq, Ord, Show)
instance (Value a, Propagator m b i) => Propagator m (a, b) (Snd i) where
    propagate (Snd i) (_, b) = propagate i b
instance (Value a, Value c, Propagator m b i) => Propagator m (a, b, c) (Snd i) where
    propagate (Snd i) (_, b, _) = propagate i b
