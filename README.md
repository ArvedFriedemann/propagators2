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
