# Propagators

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

## using the system

In the current version of the system, I recommend using the ghci with `./repl` in a terminal. When you type the function `proofSearch "Instances/<InstanceName>` you can try out one of the provided examples (Concatenation, Exchange and TypeTheory are those used in the paper)

## the code

The code looks a bit different to the one described in the paper. Main reason is that we provide additional functionality needed for debugging or tiny bugfixes. 
The unification algorithm described in the paper can be found in src/Terms/Terms.hs as the `TermListener` (don't worry about the Propagator-interface. It was used to have the propagator function attached to the pointer to have them in one place. Legacy code will probably be removed). The Andorra-Style disjunctions can be found in src/Control/Combinator/Logics.hs and the main component of the proof search is in src/Control/Language/LogLang.hs .

The Oz and Prolog instances can be found in the folders "Oz Examples" and "PROLOG Examples".

Please excuse if the code looks a bit messy, a team member left the project unexpectedly and I am still compensating. By the time your reviews will reach me however, there is a good chance that I have caught up (same with optimisations).

