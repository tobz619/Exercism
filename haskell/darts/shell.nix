{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc928", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hspec, lib }:
      mkDerivation {
        pname = "darts";
        version = "1.0.0.0";
        src = ./.;
        libraryHaskellDepends = [ base ];
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
