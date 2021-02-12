module Tests.TestLogic where

import "base" Data.List

import "criterion" Criterion.Main

import "this" Data.Terms
import "this" Control.Propagator
import "this" Control.Propagator.Event


runTestSEB :: Identifier i (TermSet i) => SEB [i] -> IO ()
runTestSEB p = defaultMain . pure . bench "testSEB" . whnfIO $ do
    
    res <- evalSEB p showAll
    putStrLn res

showAll :: (Monad m, MonadProp m, Identifier i (TermSet i)) => [i] -> m String
showAll = fmap (intercalate "\n\n") . traverse (fmap show . fromCellSize 100)
