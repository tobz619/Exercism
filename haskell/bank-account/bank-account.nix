{ mkDerivation, base, containers, hspec, lib, mtl, stm }:
mkDerivation {
  pname = "bank-account";
  version = "0.1.0.4";
  src = ./.;
  libraryHaskellDepends = [ base containers mtl stm ];
  testHaskellDepends = [ base containers hspec ];
  license = "unknown";
}
