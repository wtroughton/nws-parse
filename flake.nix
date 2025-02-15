{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];
      perSystem =
        { pkgs, system, ... }:
        let
          haskellPackages = pkgs.haskellPackages;
        in
        {

          devShells.default = pkgs.mkShell {

            buildInputs = [
              pkgs.zlib
              haskellPackages.cabal-install
              haskellPackages.ghc

              haskellPackages.ghcid
              haskellPackages.fourmolu
            ];
          };

          formatter = pkgs.nixfmt-rfc-style;

        };
    };
}
