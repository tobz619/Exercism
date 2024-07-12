{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "dominoes";
  version = "2.1.0.9";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = "unknown";
}
