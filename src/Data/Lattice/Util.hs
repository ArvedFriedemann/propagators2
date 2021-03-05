module Data.Lattice.Util where

import "this" Data.Lattice.Class

--return whether the value changed through the meet. SECOND val is the original one, second the added value.
meetDiff :: (Meet a, Eq a) => a -> a -> (a,Bool)
meetDiff new orig = (mt, orig == mt)
  where mt = new /\ orig
