{ mkDerivation, base, containers, hspec, lib, megaparsec, mtl }:
mkDerivation {
  pname = "alphametics";
  version = "1.3.0.6";
  src = /home/tobioloke/Exercism/haskell/alphametics;
  libraryHaskellDepends = [ base containers megaparsec mtl ];
  testHaskellDepends = [ base hspec ];
  license = "unknown";
}
