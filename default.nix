let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in

with pkgs;
rec {
  tool = haskellPackages.callCabal2nix "writing" ./generator {};
  site = stdenv.mkDerivation {
    name = "writing-site";
    #src = fetchgit { url = ./.; sha256 = null; };
    preferLocalBuild = true;
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
