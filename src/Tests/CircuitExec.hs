{-# LANGUAGE NoImplicitPrelude #-}
module Tests.CircuitExec where

import "base" Prelude hiding ( read )
import "base" Control.Monad
import "transformers" Control.Monad.Trans.Class

import "this" Control.Propagator.Class
import "this" Control.Propagator.Implementation
import "this" Control.Propagator.References

import "this" Circuits.Basics

import "stm" Control.Concurrent.STM hiding (atomically)
import qualified "stm" Control.Concurrent.STM as STM

stdTest :: forall a. (Value a) => IOSTMProp [CellPtr STM Ref a] -> IO ()
stdTest act = void $ runMonadPropIOFin act $ \ptrs -> do
  forM_ ptrs $ \p -> do
    v <- read @IOSTMProp @(CellPtr STM Ref) @(Scope Ref) p
    ISP $ lift $ putStrLn $ show v

gate_test' :: IO ()
gate_test' = stdTest (gate_test @IOSTMProp @(CellPtr STM Ref) @(Scope Ref))
