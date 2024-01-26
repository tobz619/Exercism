{ mkDerivation, base, containers, hspec, lib }:
mkDerivation {
  pname = "go-counting";
  version = "1.0.0.4";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  testHaskellDepends = [ base containers hspec ];
  license = "unknown";
}
