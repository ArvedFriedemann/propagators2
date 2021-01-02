module Control.Propagator.StateProp where

import "base" Control.Applicative

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as S

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as M

import "transformers" Control.Monad.Trans.State.Lazy ( StateT(..), evalStateT, modify, get, put )

import "this" Data.Lattice

data Reason where
  ID :: Int -> Reason
  FORK :: Int -> Reason -> Reason
  WATCH :: Reason -> Reason
  deriving (Eq, Ord, Show)

data MemCont where
  MTOP :: MemCont
  MBOT :: MemCont
  --UNTYPED :: a -> MemCont
  deriving (Eq, Ord, Show)

instance Meet MemCont where
  _ /\ _ = undefined
instance BoundedMeet MemCont where
  top = MTOP
instance Join MemCont where
  _ \/ _ = undefined
instance BoundedJoin MemCont where
  bot = MBOT

data RState = RS {
  mem :: Map Reason MemCont
}

type RPropagatorT = StateT RState

--Sidenote: Modify is not threadsafe.
writeR :: (Monad m) => Reason -> MemCont -> RPropagatorT m ()
writeR tr x = do
  (mem -> mp) <- get
  case M.lookup tr mp of
    Nothing -> do
      put $ RS $ M.insert tr x mp
      notify tr
    Just v -> do
      let jn = (x /\ v) in
        if x == jn
          then return ()
          else do
            put $ RS $ M.insert tr jn mp
            notify tr

readR :: (Monad m) => Reason -> RPropagatorT m MemCont
readR tr = do
  (mem -> mp) <- get
  case M.lookup tr mp of
    Nothing -> do
      put $ RS $ M.insert tr top mp
      return top
    Just v -> return v

--TODO: here the events are needed for the thing to terminate.
notify :: (Monad m) => Reason -> RPropagatorT m ()
notify tr = do
  _ <- readR (WATCH tr)
  --TODO!
  --sequence_ readers
  return ()
