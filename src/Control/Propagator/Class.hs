{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Class where

import "base" Prelude hiding ( read )
import "base" Data.Typeable
import "base" Control.Monad

import "this" Control.MonadVar.MonadVar (MonadNew, MonadMutate, MonadRead)

import "this" Data.Lattice

class (Eq a, Ord a, Show a, Typeable a) => Std a
instance (Eq a, Ord a, Show a, Typeable a) => Std a

class (forall a. Eq (v a), forall a. Ord (v a), forall a. Show (v a), Typeable v) => StdPtr v
instance (forall a. Eq (v a), forall a. Ord (v a), forall a. Show (v a), Typeable v) => StdPtr v

class (HasTop a, Meet a, Eq a, Show a, Typeable a) => Value a
instance (HasTop a, Meet a, Eq a, Show a, Typeable a) => Value a

class Dep a b | a -> b

class (MonadNew m v, MonadRead m v, MonadMutate m v) => MonadVar m v
instance (MonadNew m v, MonadRead m v, MonadMutate m v) => MonadVar m v

class MonadPropSimple m v | m -> v where
  readS :: v a -> m a
  writeS ::(Value a) =>  v a -> a -> m ()
  watchS :: v a -> m () -> m ()

class (Monad m) => MonadFork m where
  fork :: m () -> m ()
  forkF :: (Foldable t) => t (m ()) -> m ()
  forkF = foldr (\m t -> fork m >> t) (return ())

class (Monad m', MonadVar m' v, MonadVar m v) => MonadAtomic v m' m | m -> m' v where
  atomically :: m' a -> m a

class Identifier i a | i -> a
{-
data Scope = Scope
  deriving (Show, Eq, Ord, Typeable)
instance Std Scope
-}
{-
read :: v a -> Mem -> (a, Mem)
write :: v a -> a -> Mem -> ((),Mem)

return :: a -> (Mem -> (a,Mem))
pure :: a -> (Mem -> (a,Mem))
(>>=) :: (Mem -> (a,Mem)) -> (a -> (Mem -> (b,Mem))) -> (Mem -> (b,Mem))
(>>=) m1 fm2 = \mem -> let (ret,mem') = m1 mem in (fm2 ret) mem'
(>>) :: (Mem -> (a,Mem)) -> (Mem -> (b,Mem)) -> (Mem -> (b,Mem))

return blub >>= write p2
read p1 >>= (\val -> write p2 val) >>
do
  v <- read p1
  write p2 v
-}

class (Show scope, Monad m) => MonadProp m v scope | m -> v, m -> scope where
  read :: (Value a) => v a -> m a
  write :: (Value a) => v a -> a -> m ()
  watch :: (Value a, Std n) => v a -> n -> m () -> m ()
  watchl :: (Value a, Std n) => [v a] -> n -> m () -> m ()
  watchl ptrs name fkt = mapM (\(i,p) -> watch p (show name ++ show i) helper_fun) aptrs
    where
      helper_fun :: m ()
      helper_fun = do
        vls <- mapM read ptrs
        when (all (not . isTop) vls) fkt
      aptrs = zip [0..] ptrs

  watch' :: (Value a, Std n) => v a -> n -> (a -> m ()) -> m ()
  watch' ptr name fkt = watch ptr name (read ptr >>= fkt)

  new :: (Identifier n a, Value a, Std n) => n -> m (v a)
  newRelative :: (Identifier n a, Value b, Value a, Std n) => v b -> n -> m (v a)
  currScopePtr :: (Value a) => v a -> m (v a)

  newScope :: (Std n) => n -> m scope
  scoped :: scope -> m () -> m ()
  parScoped :: m () -> m ()

  watchFixpoint :: (Std n) => n -> m () -> m ()
