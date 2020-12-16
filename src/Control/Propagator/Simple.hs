module Control.Propagator.Simple
    ( SimplePropagator(..)
    , runSimplePropagator
    ) where

import "base" Data.Bifunctor

import "transformers" Control.Monad.Trans.State ( State, evalState )
import "transformers" Control.Monad.Trans.Except
import "mtl" Control.Monad.State.Class ( MonadState, state, gets )

import "this" Data.Lattice
import "this" Data.Var

import "this" Control.Propagator.Class


newtype SimplePropagator a = MkSP
    { runSP :: ExceptT String (State (PoolF Var)) a
    }
  deriving newtype (Functor, Applicative, Monad, MonadState (PoolF Var))
instance MonadFail SimplePropagator where
    fail = MkSP . throwE

runSimplePropagator :: SimplePropagator a -> Either String a
runSimplePropagator = flip evalState poolF . runExceptT . runSP

data Var a = MkVar
    { _val :: a
    , _listeners :: Pool (a -> SimplePropagator ())
    }

getVar' :: Cell SimplePropagator a -> SimplePropagator (Var a)
getVar' = gets . getVarF . unSPC

setVar' :: Cell SimplePropagator a -> Var a -> SimplePropagator ()
setVar' (MkCell i) v = state $ ((),) . setVarF i v

instance PropagatorMonad SimplePropagator where
    newtype Cell SimplePropagator a = MkCell
        { unSPC :: IdF (PoolF Var) Var a
        }
      deriving newtype (Eq, Ord, Show)

    newCell a = state $ first MkCell . newVarF (MkVar a pool)
    readCell = fmap _val . getVar'
    write c setVal = do
        MkVar oldVal ls <- getVar' c
        let newVal = oldVal /\ setVal
        if oldVal /= newVal
        then do 
            setVar' c $ MkVar newVal ls
            mapM_ ($ newVal) . vars $ ls
        else pure ()
    watch c w = do
        MkVar a v <- getVar' c
        let (i, v') = newVar w v
        setVar' c $ MkVar a v'
        pure $ do
          MkVar b v'' <- getVar' c
          setVar' c . MkVar b . delVar i $ v''
