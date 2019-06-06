{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
rec {
  tool = haskellPackages.callCabal2nix "writing" ./generator {};
  site = stdenv.mkDerivation {
    name = "writing-site";
    #src = fetchgit { url = ./.; sha256 = null; };
    preferLocalBuilds = true;
    src = ./.;
    nativeBuildInputs = [ tool glibcLocales ];
    LC_ALL = "en_US.UTF-8";
    LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
    buildPhase = ''
      ${tool}/bin/writing build
    '';
    installPhase = " cp -R _site $out ";
  };
}
