module Data.Var.Pool
    ( Pool
    , pool
    , PoolF
    , poolF
    , PoolIdF
    ) where

import "base" Data.Typeable
import "base" Data.Bifunctor
import "base" Unsafe.Coerce

import "containers" Data.Map ( Map, (!) )
import "containers" Data.Map qualified as Map

import "this" Data.Var.Class

data Pool a = Pool
    { _vars :: !(Map Int a)
    , _nextId :: {-# UNPACK #-} !Int
    }

pool :: Pool a
pool = Pool Map.empty 0 

instance Vars (Pool a) a where
    newtype Id (Pool a) a = PoolId Int
      deriving newtype (Eq, Ord)

    vars = Map.elems . _vars
    getVar (PoolId i) (Pool v _) = v ! i

instance Show (Id (Pool a) a) where
    show (PoolId i) = '@' : show i

instance MutableVars (Pool a) a where
    newVar a (Pool v n) = (PoolId n, Pool (Map.insert n a v) (succ n))
    setVar (PoolId i) a (Pool v n) = Pool (Map.insert i a v) n
    delVar (PoolId i) (Pool v n) = Pool (Map.delete i v) n

instance Foldable Pool where
    foldMap f = foldMap f . _vars


data Hidden f where
    Hidden :: !(f a) -> Hidden f

unsafeFromHidden :: Hidden f -> f a
unsafeFromHidden (Hidden a) = unsafeCoerce a

newtype PoolF f = PoolF
    { unPoolF :: Pool (Hidden f)
    }

poolF :: PoolF f
poolF = PoolF pool

type PoolIdF f = IdF (PoolF f) f

instance VarsF (PoolF f) f where
    newtype IdF (PoolF f) f a = PoolIdF (Id (Pool (Hidden f)) (Hidden f))
      deriving newtype (Eq, Ord, Show)
    
    PoolIdF a =~= PoolIdF b = if a == b
        then Just (unsafeCoerce Refl)
        else Nothing
    getVarF (PoolIdF i) = unsafeFromHidden . getVar i . unPoolF

instance MutableVarsF (PoolF f) f where
    newVarF a = bimap PoolIdF PoolF . newVar (Hidden a) . unPoolF
    setVarF (PoolIdF i) a = PoolF . setVar i (Hidden a) . unPoolF
    delVarF (PoolIdF i) = PoolF . delVar i . unPoolF