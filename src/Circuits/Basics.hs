{-# LANGUAGE NoImplicitPrelude #-}
module Circuits.Basics where

import "base" Prelude hiding ( read )
import "base" Control.Monad

import "this" Control.Propagator.Class
import "this" Data.Lattice

data SimL a = LBTOP | LBBOT | VAL a
  deriving (Show, Eq, Ord, Functor)

--(<*>) :: f (a -> b) -> f a -> f b
instance Applicative SimL where
  pure = VAL
  (VAL f) <*> a = fmap f a
  _ <*> LBBOT = LBBOT
  LBBOT <*> _ = LBBOT
  LBTOP <*> _ = LBTOP

type LBool = SimL Bool
instance (Eq a) => Meet (SimL a) where
  LBTOP /\ a = a
  a /\ LBTOP = a
  LBBOT /\ _ = LBBOT
  _ /\ LBBOT = LBBOT
  (VAL a) /\ (VAL b)
    | a==b = VAL a
    | otherwise = LBBOT

instance HasTop (SimL a) where
  top = LBTOP
  isTop LBTOP = True
  isTop _ = False

data BoolId = BID String
  deriving (Show, Eq, Ord)
instance Identifier BoolId LBool

--(<$>) :: Functor f => (a -> b) -> f a -> f b
--e.g.: not <$> b
--(<*>) :: Applicative f => f (a -> b) -> f a -> f b

and_gate :: (MonadProp m v scope, StdPtr v) => v LBool -> v LBool -> v LBool -> m ()
and_gate in1 in2 out = do
  watch in1 (in1,in2,out) compute_and
  watch in2 (in1,in2,out) compute_and
  watch' out (in1,in2,out) (\vout -> do
      when (vout == VAL True) $ do
        write in1 (VAL True)
        write in2 (VAL True)
    )
  where compute_and = do
            vin1 <- read in1
            vin2 <- read in2
            write out ((&&) <$> vin1 <*> vin2)



not_gate :: (MonadProp m v scope, StdPtr v) => v LBool -> v LBool -> m ()
not_gate input output = do
  watch' input (input,output) (\vin -> do
      write output (not <$> vin)
    )
  watch' output (input,output) (\vout -> do
      write input (not <$> vout)
    )

gate_test :: (MonadProp m v scope, StdPtr v) => m [v LBool]
gate_test = do
  in1 <- new (BID ("in1"::String))
  in2 <- new (BID ("in2"::String))
  out <- new (BID ("out"::String))
  and_gate in1 in2 out
  not_out <- new (BID ("not_out"::String))
  not_gate out not_out

  write not_out (VAL False)

  return [in1,in2,out,not_out]







--
