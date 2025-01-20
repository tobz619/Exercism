{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/fa9f817df522ac294016af3d40ccff82f5fd3a63.tar.gz") {}
, shell-dir 
}:

let
  pname = builtins.toString (builtins.baseNameOf shell-dir);
  compiler = "ghc928";
in

pkgs.haskell.packages.${compiler}.shellFor rec {
  
  packages = ps: [ 
    ps.distribution-nixpkgs (ps.callPackage "${shell-dir}/${pname}.nix" { })
  ]; 

  nativeBuildInputs = with pkgs; [
    cabal-install
    cabal2nix
    ghcid
    stack
    haskell.packages.${compiler}.haskell-language-server
  ];

  shellHook = '' 
    echo "... in ${shell-dir} ..."
    echo "... updating ${pname}.nix ..."
    ${pkgs.cabal2nix}/bin/cabal2nix ${shell-dir} > ${shell-dir}/${pname}.nix  
  '';
  
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
