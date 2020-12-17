module Control.Propagator.Simple
    ( SimplePropagator(..)
    , runSimplePropagator
    , (=~~=)
    ) where

import "base" Data.Bifunctor
import "base" Data.Function
import "base" Data.Typeable

import "transformers" Control.Monad.Trans.State ( State, evalState )
import "transformers" Control.Monad.Trans.Except
import "mtl" Control.Monad.State.Class ( MonadState, state, gets )

import "this" Data.Lattice
import "this" Data.Var
import "this" Data.Iso

import "this" Control.Propagator.Class


newtype SimplePropagator a = MkSP
    { runSP :: ExceptT String (State (PoolF CellVal)) a
    }
  deriving newtype (Functor, Applicative, Monad, MonadState (PoolF CellVal))
instance MonadFail SimplePropagator where
    fail = MkSP . throwE

runSimplePropagator :: SimplePropagator a -> Either String a
runSimplePropagator = flip evalState poolF . runExceptT . runSP

type Listeners a = Pool (a -> SimplePropagator ())
type Listener a = Id (Pool (a -> SimplePropagator ())) (a -> SimplePropagator ())
data CellVal a where
    Val :: !a -> !(Listeners a) -> ![Mapping a] -> CellVal a
    Ref :: (Meet b, Ord b)
        => !(Cell SimplePropagator b) -> !(a <-> b) -> CellVal a

data Mapping a where
    Mapping :: !(Cell SimplePropagator b) -> !(b <-> a) -> !(Listeners b) -> Mapping a

getVal' :: Cell SimplePropagator a -> SimplePropagator (CellVal a)
getVal' = gets . getVarF . unSPC

setVal' :: Cell SimplePropagator a -> CellVal a -> SimplePropagator ()
setVal' (MkSPC i) v = state $ ((),) . setVarF i v

data Sub where
    Sub :: forall a
        . ( Show (Cell SimplePropagator a), Show (Listener a))
        => !(Cell SimplePropagator a) -> !(Listener a) -> Sub

instance Show Sub where
    showsPrec d (Sub c l)
        = showParen (d >= 10)
        $ shows c
        . showString " -> "
        . shows l

instance Eq Sub where
    (==) = (==) `on` show
instance Ord Sub where
    compare = compare `on` show

(=~~=) :: Cell SimplePropagator a -> Cell SimplePropagator b -> Maybe (a :~: b)
MkSPC a =~~= MkSPC b = a =~= b

instance PropagatorMonad SimplePropagator where
    newtype Cell SimplePropagator a = MkSPC
        { unSPC :: IdF (PoolF CellVal) CellVal a
        }
      deriving newtype (Eq, Ord, Show)

    newtype Subscription SimplePropagator = SPSubs
        { unsSPSubs :: [Sub]
        }
      deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

    newCell a = state $ first MkSPC . newVarF (Val a pool [])
    readCell = (readCell' =<<) . getVal'
      where
        readCell' (Val v _ _) = pure v
        readCell' (Ref c i)   = from i <$> readCell c
    write c setVal = getVal' c >>= write'
      where
        write' (Val oldVal ls ms) = do
            let newVal = oldVal /\ setVal
            if oldVal /= newVal
            then do 
                setVal' c $ Val newVal ls ms
                mapM_ ($ newVal) . vars $ ls
            else pure ()
        write' (Ref c' (f :<-> _)) = write c' $ f setVal
    watch c w = getVal' c >>= watch'
      where
        watch' (Val a v m) = do
            let (i, v') = newVar w v
            setVal' c $ Val a v' m
            pure . SPSubs . pure $ Sub c i
        watch' (Ref c' m) = watch c' $ w . from m
    cancel = mapM_ cancel' . unsSPSubs
      where
        cancel' (Sub c l) = getVal' c >>= cancelCell
          where
            cancelCell (Val a lx m) = setVal' c $ Val a (delVar l lx) m
            cancelCell (Ref refC _) = getVal' refC >>= cancelRef refC
           
            cancelRef :: Cell SimplePropagator a -> CellVal a -> SimplePropagator ()
            cancelRef _ (Ref refC _) = getVal' refC >>= cancelRef refC
            -- ^ should not happen, since references should only point to Vals
            cancelRef c' (Val a lx m) = setVal' c' $ Val a lx $ cancelMapping =<< m
           
            cancelMapping :: Mapping a -> [Mapping a]
            cancelMapping m@(Mapping c' i lx) = case c =~~= c' of
                Just Refl -> pure . Mapping c' i $ delVar l lx
                Nothing   -> pure m

{-
instance PropagatorEqMonad SimplePropagator where
    iso a b (f :<-> t) = _
-}