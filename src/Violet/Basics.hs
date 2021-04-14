{-# LANGUAGE NoImplicitPrelude #-}
module Violet.Basics where

import "base" Prelude hiding ( read )
import "base" Control.Monad

import "this" Control.Propagator.Class
import "this" Data.Lattice

import  Data.Interval

-- ITOP = keine Infos
-- IBOT = kapott
data LatticeWrapper a = ITOP | IBOT | Val a
  deriving (Show, Eq, Ord, Functor)

instance Applicative LatticeWrapper where
    pure = Val
    (Val f) <*> a = fmap f a
    _ <*> IBOT = IBOT
    IBOT <*> _ = IBOT
    ITOP <*> _ = ITOP

type RangeType = Interval Float

type RangeL = LatticeWrapper RangeType

-- TODO entsprechend andere Varianten für andere Ranges Typen bauen
instance (Ord a) => Meet (LatticeWrapper (Interval a)) where
  ITOP /\ a = a
  a /\ ITOP = a
  IBOT /\ _ = IBOT
  _ /\ IBOT = IBOT
  (Val x) /\ (Val y) = Val (hull x y)


instance HasTop (LatticeWrapper a) where
  top = ITOP
  isTop ITOP = True
  isTop _ = False

data RangeID = RID String
  deriving (Show, Eq, Ord)

instance Identifier RangeID RangeL

data PlusId = PlusId
  deriving (Show, Eq, Ord)

plus :: (MonadProp m v scope, StdPtr v) => v RangeL -> v RangeL -> v RangeL -> m ()
plus a b res = do
  watch a (PlusId,a,b) helper
  watch b (PlusId,a,b) helper
  where
    helper = do
      aval <- read a
      bval <- read b
      write res ((+) <$> aval <*> bval)


data OpId = OpId
  deriving (Show, Eq, Ord)

op :: (MonadProp m v scope, StdPtr v) => (RangeType -> RangeType -> RangeType) -> v RangeL -> v RangeL -> v RangeL -> m ()
op f a b res = do
  watch a (OpId,a,b) helper
  watch b (OpId,a,b) helper
  where
    helper = do
      aval <- read a
      bval <- read b
      when (
        (not (isTop aval)) &&
        (not (isTop bval))
        ) $ do
        write res (f <$> aval <*> bval)

data Op1Id = Op1Id
  deriving (Show, Eq, Ord)
--op1 :: (MonadProp m v scope, StdPtr v) => (RangeType -> RangeType -> RangeType) -> v RangeL -> RangeType-> v RangeL -> m ()
op1 f a c res = do
  watch a (Op1Id,a) helper
  where
    helper = do
      aval <- read a
      when (not (isTop aval)) $ do --das sollte keinen Unterschied machen...hast du mal ein f einfach so ausprobiert, ob das funktioniert?
        write res (f <$> aval <*> (Val c))

prop_test' :: (MonadProp m v scope, StdPtr v) => m [v RangeL]
prop_test' = do
  a <- new (RID ("a"::String))
  b <- new (RID ("b"::String))
  c <- new (RID ("c"::String))
  result <- new (RID ("res"::String))
  output <- new (RID ("output"::String))
  plus a b result
  op (+) result c output
  write a (Val (2 <=..<= 4))
  write b (Val (3 <=..<= 6))
  write c (Val (0 <=..<= 20))
  return [a,b,c, result, output]


sqrt_test :: (MonadProp m v scope, StdPtr v) => m [v RangeL]
sqrt_test = do
  x <- new (RID ("x"::String))
  term1 <- new (RID ("term1"::String))
  term2 <- new (RID ("term2"::String))
  term3 <- new (RID ("term3"::String))
  term4 <- new (RID ("term4"::String))
  term5 <- new (RID ("term5"::String))

  write term1 (Val (singleton 1.0))

  op1 (*) x (singleton 0.5) term2
  op1 (*) x (singleton 0.125) term3
  op1 (*) x (singleton 0.0625) term4
  op1 (*) x (singleton 0.0390625) term5


  result <- new (RID ("sqrt(x)"::String))

  --op (**) x y result
  --op1 (**) x (singleton 2.0) result

  write x (Val (0 <=..<= 10))


  return [x, term1, term2, term3, term4, term5, result]
