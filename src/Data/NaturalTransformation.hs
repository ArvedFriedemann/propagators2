{-# LANGUAGE NoImplicitPrelude #-}
module Data.NaturalTransformation where

import "base" Prelude hiding ( (.), id )
import "base" Control.Category
import "base" Data.Coerce

import "transformers" Control.Monad.Trans.Class
import "transformers" Control.Monad.Trans.Maybe
import "transformers" Control.Monad.Trans.Except
import "transformers" Control.Monad.Trans.Identity
import "transformers" Control.Monad.Trans.Reader
import "transformers" Control.Monad.Trans.State.Lazy qualified as STL
import "transformers" Control.Monad.Trans.State.Strict qualified as STS


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
    traverseF :: (Applicative g, Functor h) 
              => proxy c
              -> (forall a. c a => f a -> g (h a)) -> ff f -> g (ff h)

instance c a => FTraversable c (Struct a) where
    traverseF _ f (Struct a) = Struct <$> f a


class FApplicative c ff where
    pureF :: proxy c -> (forall a. c a => f a) -> ff f
    liftAF2 :: proxy c -> (forall a. c a => f a -> g a -> h a) -> ff f -> ff g -> ff h 

instance c a => FApplicative c (Struct a) where
    pureF _ = Struct
    liftAF2 _ f (Struct a) (Struct b) = Struct $ f a b


newtype MonadTransformer t a m = MT
    { runMonadTransformer :: t m a 
    }

class MonadTrans t => MonadTransFFunctor t where
    ntMapT :: (Functor g, Functor h) => (forall a. g a -> h a) -> t g b -> t h b

instance MonadTransFFunctor MaybeT where
    ntMapT :: forall g h b. (Functor g, Functor h) => (forall a. g a -> h a) -> MaybeT g b -> MaybeT h b
    ntMapT = coerce @(g (Maybe b) -> h (Maybe b))
instance MonadTransFFunctor (ExceptT e) where
    ntMapT :: forall g h b. (Functor g, Functor h) => (forall a. g a -> h a) -> ExceptT e g b -> ExceptT e h b
    ntMapT = coerce @(g (Either e b) -> h (Either e b))
instance MonadTransFFunctor IdentityT where
    ntMapT :: forall g h b. (Functor g, Functor h) => (forall a. g a -> h a) -> IdentityT g b -> IdentityT h b
    ntMapT = coerce @(g b -> h b)
instance MonadTransFFunctor (ReaderT r) where
    ntMapT :: forall g h b. (Functor g, Functor h) => (forall a. g a -> h a) -> ReaderT r g b -> ReaderT r h b
    ntMapT f = coerce @((r -> g b) -> (r -> h b)) $ fmap f
instance MonadTransFFunctor (STL.StateT s) where
    ntMapT :: forall g h b. (Functor g, Functor h) => (forall a. g a -> h a) -> STL.StateT s g b -> STL.StateT s h b
    ntMapT f = coerce @((s -> g (b, s)) -> (s -> h (b, s))) $ fmap f
instance MonadTransFFunctor (STS.StateT s) where
    ntMapT :: forall g h b. (Functor g, Functor h) => (forall a. g a -> h a) -> STS.StateT s g b -> STS.StateT s h b
    ntMapT f = coerce @((s -> g (b, s)) -> (s -> h (b, s))) $ fmap f

instance MonadTransFFunctor t => FFunctor Unconstrained (MonadTransformer t a) where
    ntMap _ f = MT . ntMapT f . runMonadTransformer
    
