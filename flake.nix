{
  description = "A Haskell project";
  inputs.hix.url = "git+https://git.tryp.io/tek/hix";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.hix.inputs.nixpkgs.follows = "nixpkgs";
  outputs = {hix, ...}: hix.lib.flake {
    ghcVersions = ["ghc98" "ghc910" "ghc912"];
    compiler = "ghc912";
    gen-overrides.enable = true;

    packages = {
      h2jvm = {
        src = ./.;
        cabal = {
          author = "Alexander Wood";
          build-type = "Simple";
          license = "MIT";
          license-file = "LICENSE";
          version = "0.6.0.1";
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
            "generic-lens"
            "lens >=5.0"
            "effectful-core"
            "effectful-th"
            "effectful-plugin"
            "prettyprinter"
            "split"
            "text"
            "vector"
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
            "hspec"
            "hspec-hedgehog"
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
            ];
          };
        };
      };
    };
  };
}
