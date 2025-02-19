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
          packages.default = haskellPackages.callPackage ./default.nix { };

          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs.zlib
              pkgs.cabal2nix
              haskellPackages.cabal-install
              haskellPackages.ghc

              haskellPackages.fourmolu
              haskellPackages.ghcid
              haskellPackages.haskell-language-server
            ];
          };

          formatter = pkgs.nixfmt-rfc-style;
        };
    };
}
