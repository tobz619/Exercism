{
  description = "Exercism development flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    hpkg.url = "github:nixos/nixpkgs?ref=50a7139fbd1acd4a3d4cfa695e694c529dd26f3a";
  };

  outputs = { self, nixpkgs, hpkg }: 
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    hpkgs = hpkg.legacyPackages.x86_64-linux; 
    compiler = "ghc928";
    hl = hpkgs.haskell.packages."${compiler}";

    project = name: devTools:
      hl.developPackage {
        inherit name;
        root = pkgs.lib.sourceFilesBySuffices (./. + "/${name}")  [".cabal" ".hs"];
        returnShellEnv = !(devTools == []);
      };

  in
  {
    packages.pkg = name: project name [];
   
    defaultPackage = self.package.x86_64-linux.pkg;

    devShell = name: hl.shellFor rec{
      inherit name;
      packages = p: [];

      buildInputs = (with hl; [
        pkgs.bashInteractive
        cabal-fmt
        cabal-install
        haskell-language-server
        hlint
        hoogle
        ormolu
        ghc
        ghcid
        implicit-hie
        retrie
      ]);

    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
    
    };
  };
}
