{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "darts";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = "unknown";
}
