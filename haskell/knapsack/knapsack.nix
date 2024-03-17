{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "knapsack";
  version = "1.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = "unknown";
}
