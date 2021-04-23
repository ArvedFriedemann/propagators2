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



data OpId = OpId
  deriving (Show, Eq, Ord)

op :: (MonadProp m v scope, StdPtr v) => (RangeType -> RangeType -> RangeType) -> v RangeL -> v RangeL -> v RangeL -> m ()
op f a b res = do
  watch a (OpId,a,b,res) helper
  watch b (OpId,a,b,res) helper
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
op1 :: (MonadProp m v scope, StdPtr v, Std n) => n -> (RangeType -> RangeType -> RangeType) -> v RangeL -> RangeType-> v RangeL -> m ()
op1 fname f a c res = do
  watch a (Op1Id,fname,a) helper
  where
    helper = do
      aval <- read a
      write res (f <$> aval <*> (Val c))

naive_pow :: (MonadProp m v scope, StdPtr v) => v RangeL -> Int -> v RangeL -> m (v RangeL)
naive_pow x n res
  | n == 1 = return x
  | n == 2 = op (*) x x res >> return res
  | otherwise = return x


sqrt_test :: (MonadProp m v scope, StdPtr v) => m [v RangeL]
sqrt_test = do
  x <- new (RID ("x"::String))
  term1 <- new (RID ("term1"::String))
  term2 <- new (RID ("term2"::String))

  term3_ <- new (RID ("term3_"::String))
  term3 <- new (RID ("term3_"::String))

  term4 <- new (RID ("term4"::String))
  term4_1 <- new (RID ("term4_1"::String))
  term4_2 <- new (RID ("term4_2"::String))


  term5 <- new (RID ("term5"::String))
  term5_1 <- new (RID ("term5_1"::String))
  term5_2 <- new (RID ("term5_2"::String))
  term5_3 <- new (RID ("term5_3"::String))

  write term1 (Val (singleton 1.0))

  op1 ("t2_"::String) (*) x (singleton 0.5) term2


  op1 ("t3_"::String) (*) x (singleton 0.125) term3_
  op (*) term3_ x term3

  op1 ("t4_1"::String) (*) x (singleton 0.0625) term4_1
  op (*) x x term4_2
  op (*) term4_1 term4_2 term4



  op1 ("t5"::String) (*) x (singleton 0.0390625) term5_1
  op (*) x x term5_2
  op (*) x term5_2 term5_3
  op (*) term5_1 term5_3 term5

  result_1 <- new (RID ("result1"::String))
  result_2 <- new (RID ("result2"::String))
  result_3 <- new (RID ("result3"::String))
  result <- new (RID ("sqrt(x)"::String))

  op (+) term1 term2 result_1
  op (-) result_1 term3 result_2
  op (+) result_2 term4 result_3
  op (-) result_3 term5 result
  --op (**) x y result
  --op1 (**) x (singleton 2.0) result

  write x (Val (0 <=..<= 1))


  return [x, term1, term2, term3, term4, term5, result]
