{-# LANGUAGE NoImplicitPrelude #-}
module Circuits.Basics where

import "base" Prelude hiding ( read )

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

and_gate :: (MonadProp m v a) => v LBool -> v LBool -> v LBool -> m ()
and_gate in1 in2 out = do
  watch' in1 ("Identifier1"::String) (\v -> do
        other <- read in2
        write out (v /\ other)
    )






--
