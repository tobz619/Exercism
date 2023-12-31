{ mkDerivation, base, hpack, hspec, lib }:
mkDerivation {
  pname = "matching-brackets";
  version = "2.0.0.8";
  src = ./.;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ base hspec ];
  doCheck = false;
  prePatch = "hpack";
  license = "unknown";
}
