{-# LANGUAGE NoImplicitPrelude #-}
module Tests.SimpleTests where

import "base" Prelude hiding ( read )
import "base" Data.Typeable
import "base" Data.Maybe
import "base" Debug.Trace
import "base" Control.Monad

import "this" Control.Propagator.Class
import "this" Control.Combinator
import "this" Data.Lattice
import "this" Data.Some
import "this" Data.Util
import "this" Control.Language.LogLang
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

test5 :: forall m v scope. (MonadProp m v scope, StdPtr v) => m [TermSetPtr v]
test5 = do
  (TSP t1) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) ["c",var $ GenTId @v (1::Int),var $ GenTId @v (1::Int)] --var $ GenTId @v (1::Int)
  s <- newScope ("scp"::String)
  traceM $ "oriScp pointer t1: "++show t1
  scoped s $ do
    (TSP t2) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) [var $ GenTId @v (2::Int),"c",var $ GenTId @v (2::Int)]
    ((fromJust . splitClause -> (pres, (TSP post)))) <- refreshClause ("refresh"::String) (Set.empty,[TSP t2])
    eq t1 post
    t1'<- currScopePtr t1
    t2'<- currScopePtr t2
    traceM $ "scoped pointer t1: "++show t1'++
            "\nscoped pointer t2: "++show t2'
    promoteTerm (TSP t1)
  return [TSP t1]

test6 :: forall m v scope. (MonadProp m v scope, StdPtr v) => m [TermSetPtr v]
test6 = do
  (TSP t1) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) (var $ GenTId @v (1::Int))--["c", var $ GenTId @v (1::Int), var $ GenTId @v (2::Int)]
  (TSP t2) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t2"::String)) (var $ GenTId @v (2::Int))
  s <- newScope ("scp"::String)
  disjunctForkPromote ("djf" :: String) t1 [(do
      --(TSP t2) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) ["c", var $ GenTId @v (3::Int), var $ GenTId @v (3::Int)]
      eq t1 t2 --TODO: this is the bad one
      write t1 bot
      return ()
    ),(do
      --(TSP t2) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) ["c", "a", "b"]
      --eq t1 t2
      write t1 (constTerm "c")
      watchFixpoint ("tmp"::String) $ do
        t2' <- fromCellSize 100 (TSP t2)
        t1' <- fromCellSize 100 (TSP t1)
        t2p <- currScopePtr t2
        t1p <- currScopePtr t1
        traceM $ "t1("++show t1++", "++show t1p++") is "++show t1'++"\nt2("++show t2++", "++show t2p++") is " ++show t2'
    )
    ]
  return [TSP t1, TSP t2]

test7 :: forall m v scope. (MonadProp m v scope, StdPtr v) => m [TermSetPtr v]
test7 = do
  (TSP t1) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) []--(var $ GenTId @v (1::Int))--["c", var $ GenTId @v (1::Int), var $ GenTId @v (2::Int)]
  (TSP t2) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t2"::String)) []--(var $ GenTId @v (2::Int))

  s1 <- newScope ("djf1" :: String)
  s2 <- newScope ("djf2" :: String)

  traceM $ "Base pointers: "++show t1++", "++show t2

  scoped s1 $ do
    eq t1 t2 --TODO: this is the bad one
    --write t1 bot
    t1' <- currScopePtr t1
    t2' <- currScopePtr t2
    traceM $ "Scope s1 pointers: ("++show t1++", "++show t1'++"),("++show t2++", "++show t2'++")"
    return ()

  scoped s2 $ do
    write t1 (constTerm "c")
    promoteTerm (TSP t1)
    t1' <- currScopePtr t1
    t2' <- currScopePtr t2
    traceM $ "Scope s2 pointers: ("++show t1++", "++show t1'++"),("++show t2++", "++show t2'++")"
    return ()
    {-}
    watchFixpoint ("tmp"::String) $ do
      t2' <- fromCellSize 100 (TSP t2)
      t1' <- fromCellSize 100 (TSP t1)
      traceM $ "t1 is "++show t1'++"\nt2 is " ++show t2'-}

  return [TSP t1, TSP t2]


test8 :: forall m v scope. (MonadProp m v scope, StdPtr v) => m [TermSetPtr v]
test8 = do
  (TSP t1) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t1"::String)) ("c")--["c", "d"]
  (TSP t2) <- fromVarsAsCells @_ @_ @_ @_ @(GenTId v Int) (GenTId @v ("t2"::String)) (var $ GenTId @v (1::Int))

  s1 <- newScope ("djf1" :: String)
  s2 <- newScope ("djf2" :: String)
  --s3 <- newScope ("djf3" :: String)

  scoped s1 $ do
    eq t1 t2
    --promoteTerm (TSP t2)
    --promote t2
    watch' t1 ("show_s1t1" :: String) (\v -> traceM $ "t1 in s1 is "++show v)


  scoped s2 $ do
    eq t1 t2
    --promoteTerm (TSP t2)
    promote t2
    watch' t1 ("show_s2t1" :: String) (\v -> traceM $ "t1 in s2 is "++show v)
    {-}
  scoped s3 $ do
    eq t1 t2
    --promoteTerm (TSP t2)
    --promote t2
    watch' t1 ("show_s3t1" :: String) (\v -> traceM $ "t1 in s3 is "++show v)-}

  return [TSP t2]

--
