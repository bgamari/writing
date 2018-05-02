{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
rec {
  tool = haskellPackages.callCabal2nix "writing" ./. {};
  site = stdenv.mkDerivation {
    name = "writing-site";
    #src = fetchgit { url = ./.; sha256 = null; };
    src = ./.;
    buildInputs = [ tool ];
    buildPhase = ''
      LANG=en_us.UTF-8 ${tool}/bin/writing build
    '';
    installPhase = " cp -R _site $out ";
  };
}
