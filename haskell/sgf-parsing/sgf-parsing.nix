{ mkDerivation, base, containers, hspec, lib, parsec, text }:
mkDerivation {
  pname = "sgf-parsing";
  version = "1.2.0.5";
  src = ./.;
  libraryHaskellDepends = [ base containers parsec text ];
  testHaskellDepends = [ base containers hspec text ];
  license = "unknown";
}
