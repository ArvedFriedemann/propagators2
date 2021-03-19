module Control.Language.Pipeline where

import "this" Control.Propagator.Class
--import "this" Control.Propagator.Implementation
import "this" Control.Language.LogLang
import "this" Control.Language.TermTransformations
import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Parsing.Parser

import "parsec" Text.Parsec

import "base" Data.Typeable
import "base" Debug.Trace

parseAndPerformProofSearch :: forall m v scope n. (MonadProp m v scope, Std n, StdPtr v) => n -> String -> m (TermSetPtr v)
parseAndPerformProofSearch ctx inst = do
  let parseRes = runParser (fst <$> parseKB stdlst (SCON . CUST :: String -> TermStruc String) (SVAR :: String -> TermStruc String)) () ("parseAndPerformProofSearch at "++show ctx) inst
  case parseRes of
    Left err -> error $ show err
    Right (map cleanBrackets -> terms) -> do
      traceM "KB:"
      sequence_ (traceM <$> show <$> init terms)
      traceM "Goal:"
      traceM $ show $ last terms

      (kb, goal) <- setupSearch @m @v @scope (ctx,"SetupSearch" :: String) (SCON $ CUST "->") (init terms) (last terms)
      simpleKBNetwork (ctx,"search" :: String) kb goal
      return goal

parseFileAndPerformProofSearch :: String -> IO ()
parseFileAndPerformProofSearch filename = do
  s <- readFile filename
  --runMonadPropIO $ parseAndPerformProofSearch () s
  return ()

cleanBrackets :: (Eq a) => TermStruc a -> TermStruc a
cleanBrackets = removeLrecBrackets (SCON $ CUST "(") (SCON $ CUST ")")
