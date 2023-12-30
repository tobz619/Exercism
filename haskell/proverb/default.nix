{ pkgs ? import <nixpkgs> {} }:

pkgs.haskell.packages.ghc928.callPackage ./proverb.nix { }

