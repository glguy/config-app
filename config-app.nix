{ mkDerivation, base, config-schema, config-value, ghcjs-base, ghcjs-dom, cabal-macosx
, stdenv
}:
mkDerivation {
  pname = "config-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base config-schema config-value ghcjs-base ghcjs-dom cabal-macosx
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
