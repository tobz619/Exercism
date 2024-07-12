{ pkgs ? import (fetchTarball "channel:nixos-23.11") {}
, pname ? builtins.baseNameOf (builtins.toString ./.)
}:

pkgs.haskell.packages.ghc928.shellFor {
  packages = hpkgs: [
    hpkgs.distribution-nixpkgs
    ( hpkgs.callPackage (./${pname}/${pname}.nix) {} )
  ];

  nativeBuildInputs = with pkgs; [
    cabal-install
    haskell.packages.ghc928.haskell-language-server
    cabal2nix
    stack
  ];

  shellHook = '' 
    cabal2nix ./. > ${pname}.nix  
  '';
  
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
