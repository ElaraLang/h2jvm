{
  description = "A Haskell project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    hix = {
      url = "github:tek/hix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";

    git-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    { hix, flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } (
      { ... }:
      {
        systems = import inputs.systems;
        imports = [
          inputs.treefmt-nix.flakeModule
          inputs.git-hooks-nix.flakeModule
        ];
        perSystem =
          {
            config,
            pkgs,
            lib,
            ...
          }:
          {
            treefmt = {
              programs.actionlint.enable = true;
              programs.nixfmt.enable = true;
              programs.fourmolu.enable = true;
              programs.fourmolu.package = pkgs.haskell.packages.ghc912.fourmolu;
              programs.hlint.enable = true;
              programs.hlint.package = pkgs.haskell.packages.ghc912.hlint;
              programs.yamlfmt.enable = true;
              programs.toml-sort.enable = true;
            };
            pre-commit.settings.hooks.treefmt.enable = true;
            devShells.pre-commit = config.pre-commit.devShell;
          };
        flake =
          let
            hixFlake = hix.lib.flake (
              { config, ... }:
              {
                ghcVersions = [
                  "ghc98"
                  "ghc910"
                  "ghc912"
                ];
                compiler = "ghc912";
                gen-overrides.enable = true;
                managed.enable = true;
                managed.lower.enable = true;

                packages = {
                  h2jvm = {
                    src = ./.;
                    cabal = {
                      author = "Alexander Wood";
                      build-type = "Simple";
                      license = "MIT";
                      license-file = "LICENSE";
                      version = "0.7.0.1";
                      meta = {
                        maintainer = "alexljwood24@hotmail.co.uk";
                        synopsis = "Haskell library for writing JVM bytecode in a high level format";
                      };
                    };
                    library = {
                      enable = true;
                      dependencies = [
                        "binary"
                        "bytestring"
                        "containers"
                        "lens >=5.0"
                        "effectful-core"
                        "effectful-th"
                        "effectful-plugin"
                        "prettyprinter"
                        "text"
                        "vector"
                        "witch"
                      ];
                      default-extensions = [
                        "DataKinds"
                        "GADTs"
                        "NoFieldSelectors"
                        "OverloadedRecordDot"
                        "OverloadedStrings"
                        "TypeFamilies"
                      ];
                      source-dirs = "src";
                      language = "GHC2021";
                      ghc-options = [
                        "-Wall"
                        "-Werror=missing-export-lists"
                        "-Wunused-packages"
                        "-Wno-name-shadowing"
                        "-Werror=incomplete-patterns"
                        "-fplugin=Effectful.Plugin"
                      ];
                    };
                    tests.h2jvm-test = {
                      dependencies = [
                        "binary"
                        "bytestring"
                        "h2jvm"
                        "hedgehog >=1.4"
                        "sydtest"
                        "sydtest-hedgehog"
                        "effectful"
                        "effectful-plugin"
                      ];
                      default-extensions = [
                        "DataKinds"
                        "GADTs"
                        "NoFieldSelectors"
                        "OverloadedRecordDot"
                        "OverloadedStrings"
                        "TypeFamilies"
                      ];
                      source-dirs = "test";
                      language = "GHC2021";
                      ghc-options = [
                        "-Wall"
                        "-Wunused-packages"
                        "-Wno-name-shadowing"
                      ];
                      component = {
                        other-modules = [
                          "Analyse"
                          "Builder"
                          "Convert"
                          "Util"
                          "StackMap"
                        ];
                      };
                    };
                  };
                };
              }
            );
          in
          hixFlake;
      }
    );
}
