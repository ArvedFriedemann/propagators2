{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Combinators where

import "base" Prelude hiding ( (.), id, read )
import "base" Data.Functor
import "base" Data.Typeable
import "base" Control.Category

import "this" Data.Iso
import "this" Control.Propagator.Class


--TODO: No idea whether I did this right
recursiveCall :: (MonadProp m, Identifier w a) => w -> m () -> m ()
recursiveCall = undefined -- TODO


data Id = Id deriving (Eq, Ord, Show)
class Std i => IsIso i a b where
      theIso :: i -> a <-> b
instance Typeable a => IsIso Id a a where
      theIso _ = id

data IsoTo i b = IsoTo i b deriving (Eq, Ord, Show)
instance (MonadProp m, Value a, Identifier i b, IsIso j a b) => Propagator m (IsoTo j i) a where
    propagate (IsoTo i b) = void . write b . embed (theIso i)
data IsoFrom i a = IsoFrom i a deriving (Eq, Ord, Show)
instance (MonadProp m, Value b, Identifier i a, IsIso j a b) => Propagator m (IsoFrom j i) b where
    propagate (IsoFrom i a) = void . write a . project (theIso i)

iso :: (MonadProp m, Identifier i a, Identifier j b, IsIso p a b) => i -> j -> p -> m ()
iso a b i = void $ do
    watch a $ IsoTo i b
    watch b $ IsoFrom i a

eq :: forall a i j m. (MonadProp m, Identifier i a, Identifier j a) => i -> j -> m ()
eq a b = iso a b Id

eqAll :: (MonadProp m, Foldable t, Identifier i a) => t i -> m ()
eqAll t = maybe (pure ()) void $ foldr eqAll' Nothing t
  where
    eqAll' i Nothing  = Just (pure i)
    eqAll' i (Just j) = Just (j >>= eq i >> pure i) -- $> j

push :: forall i a m. (MonadProp m, Identifier i a) => i -> i -> m i
push s = watch s . IsoTo Id

pushL :: (MonadProp m, Identifier i a) => LiftParent m -> i -> i -> m i
pushL _ _ _ = undefined -- TODO implement via liftParent in Watcher class

promote :: (MonadProp m, Identifier i a) => LiftParent m -> i -> m i
promote _ _ = undefined -- TODO implement via liftParent in Watcher class
