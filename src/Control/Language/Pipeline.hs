module Control.Language.Pipeline where

import "this" Control.Propagator
import "this" Control.Language.LogLang
import "this" Control.Language.TermTransformations
import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Data.Terms.TermId
import "this" Parsing.Parser

import "parsec" Text.Parsec

import "base" Data.Typeable
import "base" Debug.Trace

parseAndPerformProofSearch :: (MonadFail m, MonadProp m, Typeable m, Std k) => k -> String -> m [TermId]
parseAndPerformProofSearch k inst = do
  let parseRes = runParser (fst <$> parseKB stdlst (SCON . CUST :: String -> TermStruc String) (SVAR :: String -> TermStruc String)) () ("parseAndPerformProofSearch at "++show k) inst
  case parseRes of
    Left err -> error $ show err
    Right (map cleanBrackets -> terms) -> do
      traceM "KB:"
      sequence_ (traceM <$> show <$> init terms)
      traceM "Goal:"
      traceM $ show $ last terms

      (kb, goal) <- setupSearch (k,"SetupSearch" :: String) (SCON $ CUST "->") (init terms) (last terms)
      simpleKBNetwork' 3 (k,"search" :: String) kb goal
      return [goal]


cleanBrackets :: (Eq a) => TermStruc a -> TermStruc a
cleanBrackets = removeLrecBrackets (SCON $ CUST "(") (SCON $ CUST ")")
