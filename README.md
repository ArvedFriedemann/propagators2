# Propagators2

Welcome to the second iteration of our propagator based logical language! Here, we built a framework for concurrent constraint solving that is based on the theory of propagators by Radul and LVars by Kuper. To test it, we built a Prolog-like logical language for it.

The general idea is layed out in the ICFP_Paper_Authors.pdf, with a few optimizations and corrections in Haskell_Symposium_2021_Authors.pdf .This work is unpublished so far.  


A few important files:

| File | Summary |
|---|----|
| Control.Propagator.Class.hs                 | contains the general type class |
| Control.Propagator.Reference.hs             | contains the intermediate implementation |
| Control.Propagator.Implementation.hs        | contains the concrete implementation |
| Control.Language.LogLang.hs                 | contains the proof search |
| Data.Terms.Terms.hs                         | contains the unification algorithm formulated with our interface |

Input instances can be found in the ./Instances folder. Running instances include "Concatenation" and "Exchange"

## Setup

- install the [nix package-manager](https://nixos.org/guides/install-nix.html).

## run
- run `./run`
- follow instructions

## develop

Use any editor you like, I recommend vs-code.

Have a terminal open where `./watch` is running.
If you are only working on files in `./src/` this is sufficient.
To monitor the files in `./server/` run `./watch server`.

Be aware that this does only watch files inside either `./src/` or `./server/`.
If any files outside of the currently watched folder change you have to restart `./watch`.
