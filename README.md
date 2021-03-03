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
There was a full implementation of e.f.q, split and the implication handling on a different branch. It turned out to be way too impractical for actual search, but please let me know if you want to see it.

The Oz and Prolog instances can be found in the folders "Oz Examples" and "PROLOG Examples".

Please excuse if the code looks a bit messy, a team member left the project unexpectedly and I am still compensating. By the time your reviews will reach me however, there is a good chance that I have caught up (same with optimisations).

## The Language

Most of our language features work as described in the paper, however there are a few known bugs that have not been fixed yet (but will soon be).
After every clause there needs to be a semicolon. Reason is the default Token Parser that eats my precious newlines.
Don't use custom mixfix operators without associativity that still have a hole in the front (and the back might also cause problems). The parser will not terminate. 
Yes, brackets, implication, application and inequality need to be defined extra. It's not a bug, it's a feature.

## Thank you!

Thank you so much for your interest in our system! It's our little baby and we love it!
Looking forward to continuing the research and to be reading your review.



