{ pkgs ? import ./nixpkgs.pinned.nix 
}: let
  hPkgs = pkgs.haskell.packages.ghc8102;
in { inherit hPkgs; } // hPkgs.callCabal2nix "my-propagators" ../. {}
