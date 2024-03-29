{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc928", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, hspec, lib, mtl, parsec
      , text
      }:
      mkDerivation {
        pname = "forth";
        version = "1.7.1.13";
        src = ./.;
        libraryHaskellDepends = [ base containers mtl parsec text ];
        testHaskellDepends = [ base containers hspec mtl parsec text ];
        license = "unknown";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
