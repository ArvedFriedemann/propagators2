{ pkgs ? import ./nix/nixpkgs.pinned.nix 
}: let
  drv = import ./nix/build.nix { inherit pkgs; };
  shell = import ./nix/mkHaskellShell.nix { inherit pkgs; } drv;
in if pkgs.lib.inNixShell 
  then shell
  else drv
