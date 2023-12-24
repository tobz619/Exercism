{ mkDerivation, base, containers, hspec, lib, megaparsec }:
mkDerivation {
  pname = "alphametics";
  version = "1.3.0.6";
  src = ./.;
  libraryHaskellDepends = [ base containers megaparsec ];
  testHaskellDepends = [ base hspec ];
  license = "unknown";
}
