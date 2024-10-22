{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "ocr-numbers";
  version = "1.2.0.6";
  src = /home/tobioloke/Exercism/haskell/ocr-numbers;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = "unknown";
}
