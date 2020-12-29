module Control.Language.LogLang where

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Propagator.Class

type Clause m = [Cell m (TermSet m)]
type KB m = [Clause m]
