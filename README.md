# Propagators2

Terribly sorry that the system is still under maintenance! The time between the ICFP and the Symposium deadline was shorter than anticipated. We still hope you can get some interesting insides into our implementation!

A few important files:

Control.Propagator.Class.hs                 contains the general type class
Control.Propagator.Reference.hs             contains the intermediate implementation
Control.Propagator.Implementation.hs        contains the concrete implementation

Control.Language.LogLang.hs                 contains the proof search

Data.Terms.Terms.hs                         contains the unification algorithm formulated with our interface


## Setup

- install the [nix package-manager](https://nixos.org/guides/install-nix.html).

## run
- run `./run`

## develop

Use any editor you like, I recommend vs-code or vim.

Have a terminal open where `./watch` is running.
If you are only working on files in `./src/` this is sufficient.
To monitor the files in `./server/` run `./watch server`.

Be aware that this does only watch files inside either `./src/` or `./server/`.
If any files outside of the currently watched folder change you have to restart `./watch`.
