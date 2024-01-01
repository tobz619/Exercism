{ mkDerivation, base, hpack, hspec, lib }:
mkDerivation {
  pname = "rail-fence-cipher";
  version = "1.1.0.4";
  src = ./.;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ base hspec ];
  prePatch = "hpack";
  license = "unknown";
}
