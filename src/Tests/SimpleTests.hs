{-# LANGUAGE NoImplicitPrelude #-}
module Tests.SimpleTests where

import "base" Prelude hiding ( read )
import "base" Data.Typeable
import "base" Debug.Trace
import "base" Control.Monad

import "this" Control.Propagator.Class
import "this" Control.Combinator
import "this" Data.Lattice
import "this" Data.Some
import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions

import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set

data FactSet a = FS (Set a) | FSBot
  deriving (Show, Eq, Ord, Typeable)
instance (Ord a) => Meet (FactSet a) where
  FSBot /\ _ = FSBot
  _ /\ FSBot = FSBot
  (FS a) /\ (FS b) = FS $ Set.union a b
instance (Ord a) => Join (FactSet a) where
  FSBot \/ a = a
  a \/ FSBot = a
  (FS a) \/ (FS b) = FS $ Set.intersection a b
instance (Eq a) => HasTop (FactSet a) where
  top = FS Set.empty
instance (Eq a) => HasBot (FactSet a) where
  bot = FSBot

data FactSetPointer a = FSP (Some Std)
  deriving (Show, Eq, Ord, Typeable)
instance Identifier (FactSetPointer a) (FactSet a)


data MonadPointer m = MonadPointer (Some Std)
  deriving (Show, Eq, Ord)
instance Identifier (MonadPointer m) (m ())

test :: forall m v scope. (MonadProp m v scope, Typeable m, MonadFork m) => m String
test = do
  a <- new (FSP @String $ Some ("a" :: String))
  b <- new (FSP @String $ Some ("b" :: String))
  traceM "putting eq watch to a" >> watch a (MonadPointer @m (Some ("dirEq" :: String))) (read a >>= write b)
  traceM "putting trace watch to a" >> watch a (MonadPointer @m (Some ("trace" :: String)))
    (read a >>= \a'-> traceM $ "A is " ++ show a')
  traceM "putting eq watch to b" >> watch b (MonadPointer @m (Some ("trace" :: String)))
    (read b >>= \b'-> traceM $ "B is " ++ show b')

  forM ([1..10] :: [Int]) $ \i -> fork $ write a (FS $ Set.singleton ("Test" ++ show i))

  return "finished"

data GenTId v i = GenTId i
  deriving (Show, Eq, Ord)
instance Identifier (GenTId v i) (TermSet (TermSetPtr v))

test2 :: forall m v scope. (MonadProp m v scope, StdPtr v) => m [TermSetPtr v]
test2 = do
  (TSP t1) <- fromVarsAsCells (GenTId @v ("t1"::String)) [var $ GenTId @v (1::Int),var $ GenTId @v (1::Int)]
  (TSP t2) <- fromVarsAsCells (GenTId @v ("t1"::String)) ["c",var $ GenTId @v (2::Int)]
  eq t1 t2
  return [TSP t1]

test3 :: forall m v scope. (MonadProp m v scope, StdPtr v) => m [TermSetPtr v]
test3 = do
  (TSP t1) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) [var $ GenTId @v (1::Int)] --var $ GenTId @v (1::Int)
  s <- newScope ("scp"::String)
  traceM $ "oriScp pointer t1: "++show t1
  scoped s $ do
    (TSP t2) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) ["c"]
    eq t1 t2
    t1'<- currScopePtr t1
    t2'<- currScopePtr t2
    traceM $ "scoped pointer t1: "++show t1'++
            "\nscoped pointer t2: "++show t2'
    promoteTerm (TSP t1)
  return [TSP t1]

test4 :: forall m v scope. (MonadProp m v scope, StdPtr v) => m [TermSetPtr v]
test4 = do
  (TSP t1) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) ["c",var $ GenTId @v (1::Int),var $ GenTId @v (1::Int)] --var $ GenTId @v (1::Int)
  s <- newScope ("scp"::String)
  traceM $ "oriScp pointer t1: "++show t1
  scoped s $ do
    (TSP t2) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) [var $ GenTId @v (2::Int),"c",var $ GenTId @v (2::Int)]
    eq t1 t2
    t1'<- currScopePtr t1
    t2'<- currScopePtr t2
    traceM $ "scoped pointer t1: "++show t1'++
            "\nscoped pointer t2: "++show t2'
    promoteTerm (TSP t1)
  return [TSP t1]








--
