module Tests.TestLogic where

import "base" Data.List

import "this" Data.Terms
import "this" Control.Propagator
import "this" Control.Propagator.Event


runTestSEB :: Identifier i (TermSet i) => SEB [i] -> IO ()
runTestSEB p = do
    (res, st) <- runSEB p showAll
    putStrLn res
    putStrLn ""
    putStrLn . prettyPrintValues $ st

showAll :: (Monad m, MonadProp m, Identifier i (TermSet i)) => [i] -> m String
showAll = fmap (intercalate "\n\n") . traverse (fmap show . fromCellSize 100)
