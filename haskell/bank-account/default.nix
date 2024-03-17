{ pkgs ? import (fetchTarball "channel:nixos-23.11") {}
}:

let
  pname = builtins.baseNameOf (builtins.toString ./.)

in

pkgs.haskell.packages.ghc928.developPackage (./. + "/${pname}.nix") {}
