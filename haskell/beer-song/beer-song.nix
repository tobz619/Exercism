{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "beer-song";
  version = "0.1.0.3";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = "unknown";
}
