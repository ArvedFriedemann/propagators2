{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator where

import "base" Prelude hiding ( (.), id )
import "base" Data.Typeable
import "base" Data.Bifunctor
import "base" Control.Category

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

import "transformers" Control.Monad.Trans.State ( State, evalState )
import "transformers" Control.Monad.Trans.Except
import "mtl" Control.Monad.State.Class ( MonadState, state, gets, modify )

import "this" Data.Lattice


type LegalCellValue a = (Typeable a, Meet a, Ord a)
class Monad m => PropagatorMonad m where
    data Cell m a
    newCell :: LegalCellValue a => a -> m (Cell m a)
    readCell :: LegalCellValue a => Cell m a -> m a
    write :: LegalCellValue a => Cell m a -> a -> m ()
    watch :: LegalCellValue a => Cell m a -> (a -> m ()) -> m ()

linkM :: (PropagatorMonad m, LegalCellValue a, LegalCellValue b) => Cell m a -> Cell m b -> (a -> m b) -> m ()
linkM ca cb f = watch ca $ \ a -> f a >>= write cb

link :: (PropagatorMonad m, LegalCellValue a, LegalCellValue b) => Cell m a -> Cell m b -> (a -> b) -> m ()
link ca cb f = linkM ca cb $ pure . f

link2 :: (PropagatorMonad m, LegalCellValue a, LegalCellValue b, LegalCellValue c) => Cell m a -> Cell m b -> Cell m c -> (a -> b -> c) -> m ()
link2 ca cb cc f = do
    linkM ca cc $ \ a -> f a <$> readCell cb
    linkM cb cc $ \ b -> flip f b <$> readCell ca
    
newEmptyCell :: (PropagatorMonad m, LegalCellValue a, BoundedMeet a) => m (Cell m a)
newEmptyCell = newCell upperBound

data Val m where
    Val :: Typeable a => (a, a -> m ()) -> Val m
val :: (Applicative m, Typeable a) => a -> Val m
val = Val . (, const . pure $ ())
unVal :: (Typeable a, Typeable m) => Val m -> Maybe (a, a -> m ())
unVal (Val v) = cast v

newtype MyProp a = MyProp (ExceptT String (State (Map Int (Val MyProp), Int)) a)
  deriving newtype (Functor, Applicative, Monad, MonadState (Map Int (Val MyProp), Int))
instance MonadFail MyProp where
    fail = MyProp . throwE

runMyProp :: MyProp a -> Either String a
runMyProp (MyProp m) = evalState (runExceptT m) (Map.empty, 0)

class HasVal m i a | m i -> a where
    getVal :: i -> m a
    putVal :: i -> a -> m ()
instance (Typeable a, Ord a) => HasVal MyProp (Cell MyProp a) (a, a -> MyProp ()) where
    getVal (MyCell i) = do
        Just v <- MyProp . gets $ (unVal =<<) . Map.lookup i . fst
        pure v
    putVal (MyCell i) = MyProp . modify . first . Map.insert i . Val  

nextCell :: Typeable a => MyProp (Cell MyProp a)
nextCell = MyProp . state $ \ (m, i) -> (MyCell i, (m, succ i))

instance PropagatorMonad MyProp where
    newtype Cell MyProp a = MyCell Int
    newCell a = do
        c <- nextCell
        putVal c (a, const . pure $ ())
        pure c
    readCell = fmap fst . getVal
    write c a' = do
        (a, w) <- getVal c
        let a'' = a /\ a'
        if a /= a''
        then do 
            putVal c (a'', w)
            w a''
        else pure ()
    watch c w = do
        (a, v) <- getVal c
        putVal c (a, \ x -> v x *> w x)
