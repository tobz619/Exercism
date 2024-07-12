{ mkDerivation, base, hspec, lib, QuickCheck }:
mkDerivation {
  pname = "allergies";
  version = "1.2.0.7";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec QuickCheck ];
  license = "unknown";
}
