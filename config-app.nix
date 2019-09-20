{ mkDerivation, base, config-schema, config-value, ghcjs-base, ghcjs-dom, cabal-macosx, pretty-show
, stdenv, lib
}:
with lib;
mkDerivation {
  pname = "config-app";
  version = "0.1.0.0";
  src = cleanSourceWith {
   src = ./.;
   filter =
    name: type: let baseName = baseNameOf (toString name); in (
      (type == "regular" && hasSuffix ".hs" baseName) ||
      (hasSuffix ".yaml" baseName) ||
      (hasSuffix ".css" baseName) ||
      (hasSuffix ".png" baseName) ||
      (hasSuffix ".js" baseName) ||
      (baseName == "README.md") ||
      (baseName == "LICENSE") ||
      (type == "directory" && baseName != "dist")
    );
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base config-schema config-value ghcjs-base ghcjs-dom cabal-macosx pretty-show
  ];
  homepage = "galois.com";
  description = "Interactive configuration parsing";
  license = stdenv.lib.licenses.bsd3;
  postInstall = ''
    cp index.html $out
    cp prism.css  $out
    cp prism.js   $out
  '';
}
