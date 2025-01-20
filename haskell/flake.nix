{
  description = "Exercism development flake";

  inputs = {
    hpkg.url = "github:nixos/nixpkgs?ref=50a7139fbd1acd4a3d4cfa695e694c529dd26f3a";
  };

  outputs = { self, hpkg}: 
  let
    pname = builtins.toString (builtins.baseNameOf ./.);
    hpkgs = hpkg.legacyPackages.x86_64-linux; 
    compiler = "ghc928";
    hl = hpkgs.haskell.packages."${compiler}";

    project = name: devTools:
      hl.developPackage {
        inherit name;
        root = hpkgs.lib.sourceFilesBySuffices (./. + "/${name}")  [".cabal" ".hs"];
        returnShellEnv = !(devTools == []);
      };

  in
 rec {
    packages.pkg = project pname devShell.buildInputs;
   
    # defaultPackage = nixpkgs.packages.x86_64-linux;

    devShell = hpkgs.callPackage ./shellmaker.nix {pkgs = hpkgs; shell-dir = (builtins.toString ./. + "/${pname}"); };

    LD_LIBRARY_PATH = hpkgs.lib.makeLibraryPath devShell.buildInputs;
    
    
  };
}
