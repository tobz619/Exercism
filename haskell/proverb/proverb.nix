{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "proverb";
  version = "1.1.0.2";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = "unknown";
}
