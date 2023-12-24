{ nixpkgs ? import <nixpkgs> {}, compiler ? ghc928 , doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, hspec, lib, megaparsec }:
      mkDerivation {
        pname = "alphametics";
        version = "1.3.0.6";
        src = ./.;
        libraryHaskellDepends = [ base containers megaparsec ];
        testHaskellDepends = [ base hspec ];
        license = "unknown";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
