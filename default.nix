{ pkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
with pkgs.haskell.lib;
let
  call = pkgs.haskell.packages.${compiler}.callCabal2nix;
  kan-extensions = call "kan-extensions" (pkgs.fetchFromGitHub {
        owner = "ekmett";
        repo = "kan-extensions";
        rev = "eb77e603597fddf98998d8d27e8ffa18f3f42117";
        sha256 = "0xlld50nw1l6imdq74sli8dfl8yh7kf3kp8k71f3q8729xq6cmmw";
  }) {};
  config-value = call "config-value" (pkgs.fetchFromGitHub {
        owner = "glguy";
        repo = "config-value";
        rev = "9f6960b2d49ef3c39660ce7e2ac8aa9893109cf9";
        sha256 = "15hl129428n6mrklaqr3j2yxnbms9n4ikr9yp4s4vz6ir3zhh5r7";
  }) {};
  config-schema = dontHaddock (call "config-schema" (pkgs.fetchFromGitHub {
        owner = "glguy";
        repo = "config-schema";
        rev = "4ec0f7a5f5e5e174749143b5d7ebaa916b8394b0";
        sha256 = "094zdw1llrl8qxis1zlqm4rqmkyi6hpiqivlqfrh0n3zm5x5vzjp";
  }) { inherit config-value kan-extensions; });
 app = pkgs.haskell.packages.${compiler}.callPackage ./config-app.nix {
   inherit config-value config-schema;
 };
in
  pkgs.runCommand "app" { inherit app; } ''
    mkdir $out
    cp ${app}/index.html $out/
    cp ${app}/prism.css $out/
    cp ${app}/prism.js $out/
    cp ${app}/bin/config-app-js.jsexe/*.js $out/
  ''
