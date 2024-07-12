{ pkgs ? import (fetchTarball "channel:nixos-23.11") {}
, pname ? builtins.baseNameOf (builtins.toString ./.)
}:

pkgs.haskell.packages.ghc928.developPackage (./. + "/${pname}.nix") {}
