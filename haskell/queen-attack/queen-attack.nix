{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "queen-attack";
  version = "2.2.0.7";
  src = /nix/store/ipk8zvcvdz1l0s37fl17nd3yidlilh2i-queen-attack;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = "unknown";
}
