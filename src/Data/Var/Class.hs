module Data.Var.Class where

import "base" Data.Typeable


class (Ord (Id v a), Show (Id v a)) => Vars v a | v -> a where
    data Id v a
    vars :: v -> [a]
    getVar :: Id v a -> v -> a

class Vars v a => MutableVars v a | v -> a where
    newVar :: a -> v -> (Id v a, v)
    setVar :: Id v a -> a -> v -> v
    delVar :: Id v a -> v -> v
    updateVar :: Functor t => Id v a -> (a -> t a) -> v -> t v
    updateVar i f v = flip (setVar i) v <$> (f . getVar i $ v)


class (forall a. Ord (IdF v f a), forall a. Show (IdF v f a))
        => VarsF v f | v -> f where
    data IdF v f :: * -> *
    (=~=) :: IdF v f a -> IdF v f b -> Maybe (a :~: b)
    getVarF :: IdF v f a -> v -> f a

class VarsF v f => MutableVarsF v f | v -> f where
    newVarF :: f a -> v -> (IdF v f a, v)
    setVarF :: IdF v f a -> f a -> v -> v
    delVarF :: IdF v f a -> v -> v
    updateVarF :: Functor t => IdF v f a -> (f a -> t (f a)) -> v -> t v
    updateVarF i f v = flip (setVarF i) v <$> (f . getVarF i $ v)
