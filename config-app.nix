{ mkDerivation, base, config-schema, config-value, ghcjs-base
, stdenv
}:
mkDerivation {
  pname = "config-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base config-schema config-value ghcjs-base
  ];
  homepage = "galois.com";
  description = "Interactive configuration parsing";
  license = stdenv.lib.licenses.bsd3;
}
