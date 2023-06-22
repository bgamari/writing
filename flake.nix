{
  description = "Ben's bloggish thing";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-20.03";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages.tool = pkgs.haskellPackages.callCabal2nix "writing" ./generator {};
        packages.site = pkgs.stdenv.mkDerivation {
          name = "writing-site";
          preferLocalBuild = true;
          src = ./.;
          nativeBuildInputs = [ packages.tool pkgs.glibcLocales ];
          LC_ALL = "en_US.UTF-8";
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
          buildPhase = ''
            ${packages.tool}/bin/writing build
          '';
          installPhase = "cp -R _site $out";
        };

        packages.default = packages.site;

        apps.default = flake-utils.lib.mkApp {
          drv = packages.tool;
        };
      });
}
