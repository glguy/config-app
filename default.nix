{ pkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
with pkgs.haskell.lib;
let
  call = pkgs.haskell.packages.${compiler}.callCabal2nix;
  kan-extensions = call "config-value" (pkgs.fetchFromGitHub {
        owner = "ekmett";
        repo = "kan-extensions";
        rev = "eb77e603597fddf98998d8d27e8ffa18f3f42117";
        sha256 = "0xlld50nw1l6imdq74sli8dfl8yh7kf3kp8k71f3q8729xq6cmmw";
  }) {};
  config-value = call "config-value" (pkgs.fetchFromGitHub {
        owner = "glguy";
        repo = "config-value";
        rev = "ecaa9de295457a60a767adbfa29eac2056f178bd";
        sha256 = "01d6axwaj786np6c8xv9rg3vf3ynps0bkwv3964v2rif9flpw1h2";
  }) {};
  config-schema = dontHaddock (call "config-schema" (pkgs.fetchFromGitHub {
        owner = "glguy";
        repo = "config-schema";
        rev = "a5495ac688159a6c80bfb1508322db1126213494";
        sha256 = "0a3m9k2yy7hq1kzcwmdrxjrqb3dlja6cwrqxfrbbi3cyc2pn90p7";
  }) { inherit config-value kan-extensions; });
 app = pkgs.haskell.packages.${compiler}.callPackage ./config-app.nix {
   inherit config-value config-schema;
 };
in
  pkgs.runCommand "app" { inherit app; } ''
    mkdir $out
    cp ${app}/index.html $out/index.html
    cp ${app}/bin/config-app.jsexe/all.js $out/all.js
  ''
