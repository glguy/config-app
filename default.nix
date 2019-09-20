with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
{ nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }
, compiler ? "ghcjs86"
}:
let
  packageOverrides = pkgs: {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc864 = pkgs.haskell.packages.ghc864.override {
          overrides = self: super: with pkgs.haskell.lib; {
            happy = dontCheck (super.callHackage "happy" "1.19.9" {});
            mkDerivation = args: super.mkDerivation (args // {
              enableLibraryProfiling = false;
              doCheck = false;
              doHaddock = false;
            });
          };
        };
        ghcjs86 = pkgs.haskell.packages.ghcjs86.override {
          overrides = import ./ghcjs.nix pkgs;
        };
      };
    };
  };
  pkgs = import nixpkgs { config.packageOverrides = packageOverrides; };
  ghcjsPkgs = pkgs.haskell.packages.${compiler};
in
  with ghcjsPkgs;
  pkgs.stdenv.mkDerivation {
    name = "app";
    buildInputs = [ config-app ];
    buildCommand = ''
      mkdir $out
      cp ${config-app}/index.html $out/
      cp ${config-app}/prism.css $out/
      cp ${config-app}/prism.js $out/
      cp ${config-app}/bin/config-app-js.jsexe/*.js $out/
    '';
   }
