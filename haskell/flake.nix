{
  description = "Exercism development flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    compiler = "ghc928";
    hl = pkgs.haskell.packages."${compiler}";

    project = name: devTools:
      hl.developPackage {
        inherit name;
        root = pkgs.lib.sourceFilesBySuffices ./. [".cabal" ".hs"];
        returnShellEnv = !(devTools == []);
      };

  in
  {
    packages.pkg = name: project name [];
   
    defaultPackage = self.package.x86_64-linux.pkg;

    devShell = name: project name (with hl; [
      cabal-fmt
      cabal-install
      haskell-language-server
      hlint
      stack
    ]); 
  };
}
