# Propagators2

Welcome to the second iteration of our propagator based logical language! Here, we built a framework for concurrent constraint solving that is based on the theory of propagators by Radul and LVars by Kuper. To test it, we built a Prolog-like logical language for it.

A few important files:

Control.Propagator.Class.hs                 contains the general type class
Control.Propagator.Reference.hs             contains the intermediate implementation
Control.Propagator.Implementation.hs        contains the concrete implementation

Control.Language.LogLang.hs                 contains the proof search

Data.Terms.Terms.hs                         contains the unification algorithm formulated with our interface

Input instances can be found in the ./Instances folder. Running instances include "Concatenation" and "Exchange"

## Setup

- stack build

## run
- stack run
- follow instructions
