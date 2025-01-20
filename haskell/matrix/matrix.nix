{ mkDerivation, base, hspec, lib, megaparsec, QuickCheck, vector }:
mkDerivation {
  pname = "matrix";
  version = "1.3.0.9";
  src = /home/tobioloke/Exercism/haskell/matrix;
  libraryHaskellDepends = [ base megaparsec vector ];
  testHaskellDepends = [ base hspec QuickCheck vector ];
  license = "unknown";
}
