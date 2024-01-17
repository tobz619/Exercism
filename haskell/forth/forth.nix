{ mkDerivation, base, containers, hspec, lib, mtl, parsec, text }:
mkDerivation {
  pname = "forth";
  version = "1.7.1.13";
  src = ./.;
  libraryHaskellDepends = [ base containers mtl parsec text ];
  testHaskellDepends = [ base containers hspec mtl parsec text ];
  license = "unknown";
}
