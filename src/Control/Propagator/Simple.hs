{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Simple
    ( SimplePropagator(..)
    , runSimplePropagator
    , (=~~=)
    ) where

import "base" Prelude hiding ( (.), id )
import "base" Data.Bifunctor
import "base" Data.Function ( on )
import "base" Data.Typeable
import "base" Control.Category
import "base" Control.Applicative
import "base" Control.Monad.Fix
import "base" Control.Monad

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
  deriving newtype 
    ( Functor, Applicative, Monad
    , MonadState (PoolF CellVal)
    , MonadFix
    , Alternative, MonadPlus
    )
instance MonadFail SimplePropagator where
    fail = MkSP . throwE

runSimplePropagator :: SimplePropagator a -> Either String a
runSimplePropagator = flip evalState poolF . runExceptT . runSP

type Listeners a = Pool (a -> SimplePropagator ())
type Listener a = Id (Pool (a -> SimplePropagator ())) (a -> SimplePropagator ())
data CellVal a where
    Val :: !a -> !(Listeners a) -> ![Mapping a] -> CellVal a
    Ref :: (Meet b, Ord b)
        => !(Cell SimplePropagator b) -> !(b <-> a) -> CellVal a

data Mapping a where
    Mapping :: !(Cell SimplePropagator b) -> !(b <-> a) -> !(Listeners b) -> Mapping a

getVal' :: Cell SimplePropagator a -> SimplePropagator (CellVal a)
getVal' = gets . getVarF . unSPC

setVal' :: Cell SimplePropagator a -> CellVal a -> SimplePropagator ()
setVal' (MkSPC _ i) v = state $ ((),) . setVarF i v

callListeners :: (Ord a, Meet a) => Cell SimplePropagator a -> a -> SimplePropagator ()
callListeners c a = getVal' c >>= flip callListeners' a

callListeners' :: (Ord a, Meet a) => CellVal a -> a -> SimplePropagator ()
callListeners' (Ref c i) a = callListeners c $ from i a
callListeners' (Val _ ls ms) a = do
    execListeners a ls
    mapM_ (callMapping a) ms
  where
    execListeners newVal = mapM_ ($ newVal) . vars
    callMapping newVal (Mapping _ i mls) = execListeners (from i newVal) mls

data Sub where
    Sub :: forall a
        . ( Show (Cell SimplePropagator a), Show (Listener a))
        => !(Cell SimplePropagator a) -> !(Listener a) -> Sub

instance Show Sub where
    showsPrec d (Sub c l)
        = showParen (d > 10)
        $ shows c
        . showString " -> "
        . shows l

instance Eq Sub where
    (==) = (==) `on` show
instance Ord Sub where
    compare = compare `on` show

instance Show (Cell SimplePropagator a) where
    showsPrec _ c
        = showString (cellName c)
        . shows (unSPC c)

(=~~=) :: Cell SimplePropagator a -> Cell SimplePropagator b -> Maybe (a :~: b)
MkSPC _ a =~~= MkSPC _ b = a =~= b

instance PropagatorMonad SimplePropagator where
    data Cell SimplePropagator a = MkSPC
        { cellName :: String
        , unSPC :: IdF (PoolF CellVal) CellVal a
        }
      deriving (Eq, Ord)

    newtype Subscription SimplePropagator = SPSubs
        { unsSPSubs :: [Sub]
        }
      deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

    newCell n a = state $ first (MkSPC n) . newVarF (Val a pool [])
    readCell = (readCell' =<<) . getVal'
      where
        readCell' (Val v _ _) = pure v
        readCell' (Ref c i)   = to i <$> readCell c

    write c setVal = getVal' c >>= write'
      where
        write' (Val oldVal ls ms) = do
            let newVal = oldVal /\ setVal
            if oldVal /= newVal
            then do 
                setVal' c $ Val newVal ls ms
                callListeners c newVal
            else pure ()
        write' (Ref c' i) = write c' . from i $ setVal

    watch c w = getVal' c >>= watch'
      where
        watch' (Val a v m) = do
            let (i, v') = newVar w v
            setVal' c $ Val a v' m
            callListeners c a
            pure . SPSubs . pure $ Sub c i
        watch' (Ref c' m) = watch c' $ w . to m

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


instance PropagatorEqMonad SimplePropagator where
    iso :: forall a b. (Ord a, Meet a, Ord b, Meet b)
        => Cell SimplePropagator a -> Cell SimplePropagator b -> (a <-> b) -> SimplePropagator ()
    iso a b i = do
        va <- getVal' a
        vb <- getVal' b
        iso' va vb
      where
        iso' :: CellVal a -> CellVal b -> SimplePropagator ()
        iso' (Ref refA i') _ = iso refA b $ i . i'
        iso' _ (Ref refB i') = iso a refB $ co i' . i
        iso' (Val av alx am) (Val bv blx bm) = case a =~~= b of
            Just Refl -> pure () -- already equal. we assume that i = id
            Nothing   -> do
                let v = av /\ from i bv
                setVal' a $ Val v alx (am <> [Mapping b (co i) blx] <> fmap moveMapping bm)
                callListeners a v

        moveMapping (Mapping ref i' lx) = Mapping ref (co i . i') lx
