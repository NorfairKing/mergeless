{
  description = "mergeless";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    nixpkgs-22_05.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    nixpkgs-21_11.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
    nixpkgs-21_05.url = "github:NixOS/nixpkgs?ref=nixos-21.05";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22_05
    , nixpkgs-21_11
    , nixpkgs-21_05

    , flake-utils
    , pre-commit-hooks
    , gitignore
    , validity
    , safe-coloured-text
    , autodocodec
    , sydtest
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
      let
        pkgsFor = nixpkgs: import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.${system}
            (import (validity + "/nix/overlay.nix"))
            (import (autodocodec + "/nix/overlay.nix"))
            (import (safe-coloured-text + "/nix/overlay.nix"))
            (import (sydtest + "/nix/overlay.nix"))
            (final: previous: { inherit (import gitignore { inherit (final) lib; }) gitignoreSource; })
          ];
        };
        pkgs = pkgsFor nixpkgs;
      in
      {
        overlays = import ./nix/overlay.nix;
        packages.release = pkgs.haskellPackages.mergelessRelease;
        packages.default = self.packages.${system}.release;
        checks =
          let
            backwardCompatibilityCheckFor = nixpkgs:
              let pkgs' = pkgsFor nixpkgs;
              in pkgs'.haskellPackages.mergelessRelease;
            allNixpkgs = {
              inherit
                nixpkgs-22_05
                nixpkgs-21_11
                nixpkgs-21_05;
            };
            backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
          in
          backwardCompatibilityChecks // {
            pre-commit = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                hlint.enable = true;
                hpack.enable = true;
                ormolu.enable = true;
                nixpkgs-fmt.enable = true;
                nixpkgs-fmt.excludes = [ ".*/default.nix" ];
                cabal2nix.enable = true;
              };
            };
          };
        devShells.default = pkgs.haskellPackages.shellFor {
          name = "mergeless-shell";
          packages = (p:
            (builtins.attrValues p.mergelessPackages)
          );
          withHoogle = true;
          doBenchmark = true;
          buildInputs = with pkgs; [
            niv
            zlib
            cabal-install
          ] ++ (with pre-commit-hooks;
            [
              hlint
              hpack
              nixpkgs-fmt
              ormolu
              cabal2nix
            ]);
          shellHook = self.checks.${system}.pre-commit.shellHook;
        };
      });
}
