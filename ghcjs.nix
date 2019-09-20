pkgs:
let
  ghcjs-dom-src = pkgs.fetchFromGitHub {
    owner  = "ghcjs";
    repo   = "ghcjs-dom";
    rev    = "b8e483a";
    sha256 = "06qlbbhjd0mlv5cymp5q0rb69a333l0fcif5zwa83h94dh25c1g7";
  };
  kan-extensions-src = pkgs.fetchFromGitHub {
    owner  = "ekmett";
    repo   = "kan-extensions";
    rev    = "f9f8717";
    sha256 = "1jqgn3dx0gvcq277k1ll2wwynxr0fcwgpbk0z11rj5yr9f0n3f76";
  };
  config-value-src = pkgs.fetchFromGitHub {
    owner  = "glguy";
    repo   = "config-value";
    rev    = "d4ebfa6";
    sha256 = "0jpz6kp4lxjc8yg8q1z2mwr2ipsshyzfz8dmsssp2ncmxjqkj1sm";
  };
  config-schema-src = pkgs.fetchFromGitHub {
    owner  = "glguy";
    repo   = "config-schema";
    rev    = "163ae63";
    sha256 = "1jqgn3dx0gvcq277k1ll2wwynxr0fcwgpbk0z11rj5yr9f0n3f76";
  };
  happy-src = pkgs.fetchFromGitHub {
    owner  = "simonmar";
    repo   = "happy";
    rev    = "3ea1aa7";
    sha256 = "08gmmya416i0axfxqr702wp71a06cbly1757r8w772xa77swa0vl";
  };
in
  self: super: with pkgs.haskell.lib; {
    ghcjs-dom       = self.callCabal2nix "ghcjs-dom" "${ghcjs-dom-src}/ghcjs-dom" {};
    ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${ghcjs-dom-src}/ghcjs-dom-jsffi" {};
    kan-extensions  = self.callCabal2nix "kan-extensions" kan-extensions-src {};
    config-value    = addBuildTool (self.callCabal2nix "config-value" config-value-src {}) pkgs.haskellPackages.happy_1_19_5;
    config-schmea   = self.callCabal2nix "config-schema" config-schema-src {};
    happy           = addBuildTool (self.callCabal2nix "happy" happy-src {}) pkgs.haskellPackages.happy;
    config-app      = self.callPackage ./config-app.nix {};
    mkDerivation    = args: super.mkDerivation (args // { doCheck = false; });
    doctest         = null;
  }
