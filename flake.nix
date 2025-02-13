{
  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    horizon-platform.url = "github:wtroughton/horizon-platform";
    horizon-devtools.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-devtools";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = 
    inputs@
    { self
    , flake-parts
    , horizon-devtools
    , horizon-platform
    , nixpkgs
    , ...
    }:

    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      perSystem = { pkgs, system, ... }:
        let

          myOverlay = final: prev: { nws-parse = final.callCabal2nix "nws-parse" ./. { }; };

          legacyPackages = horizon-platform.legacyPackages.${system}.extend myOverlay;

        in
        {

          devShells.default = legacyPackages.nws-parse.env.overrideAttrs (attrs: {
            buildInputs = attrs.buildInputs ++ [
              legacyPackages.cabal-install
              horizon-devtools.legacyPackages.${system}.ghcid
            ];
          });

          packages.default = legacyPackages.nws-parse;

        };
    };
}
