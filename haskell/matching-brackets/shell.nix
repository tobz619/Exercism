{ pkgs ? import (fetchTarball "channel:nixos-23.11") {}
}:

pkgs.haskell.packages.ghc928.shellFor {
  packages = hpkgs: [
    hpkgs.distribution-nixpkgs
    ( hpkgs.callPackage ./matching-brackets.nix {} )
  ];

  nativeBuildInputs = with pkgs; [
    cabal-install
    haskell.packages.ghc928.haskell-language-server
    cabal2nix
    stack
  ];

  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
