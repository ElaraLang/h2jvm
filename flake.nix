{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";

  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {

      systems = import inputs.systems;

      imports = with inputs; [
        haskell-flake.flakeModule
        treefmt-nix.flakeModule
        flake-root.flakeModule
        mission-control.flakeModule
      ];

      perSystem = { self', config, pkgs, ... }: {

        haskellProjects.default = {

          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell

          basePackages = pkgs.haskell.packages.ghc98;

          devShell = {
            tools = hp: {
              treefmt = config.treefmt.build.wrapper;
            } // config.treefmt.build.programs;

            hlsCheck.enable = false;
          };

          packages = {
            # fourmolu.source = "0.11.0.0";
            hedgehog.source = "1.4";
            tasty-hedgehog.source = "1.4.0.2";
            hlint.source = "3.8";
          };

          settings = {
            ghcid = {
              separateBinOutput = false;
              check = false;
            };
            fourmolu.check = false;
            hw-fingertree.check = false;
            hw-prim.check = false;
          };
        };



        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = false;

          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu.options = [
            "--ghc-opt"
            "-XImportQualifiedPost"
          ];
        };

        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };

          fmt = {
            description = "Format the source tree";
            exec = config.treefmt.build.wrapper;
            category = "Dev Tools";
          };

          run = {
            description = "Run the project with ghcid auto-recompile";
            exec = ''
              ghcid -c "cabal repl h2jvm:test:h2jvm-test" --warnings -T :main --colour=always
            '';
            category = "Primary";
          };
        };


        packages.default = self'.packages.h2jvm;

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.mission-control.devShell
          ];

        };

      };
    };
}

