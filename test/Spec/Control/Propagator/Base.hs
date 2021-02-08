module Spec.Control.Propagator.Base where

import "tasty" Test.Tasty


tests :: TestTree
tests = testGroup "Control.Propagator.Event.EventT"
    [
    ]
{-
class Reducible m where
  reduce :: m a -> (a -> m b) -> b

testWrite :: (MonadProp m, Reducible m, Identifier () Int, BoundedJoin Int) -> [Int] -> Bool
testWrite [] = reduce return (const $ read ()) == top
testWrite is = reduce m (const $ read ()) == last is
  where m = do
    sequence_ $ (write ()) <$> is
-}
