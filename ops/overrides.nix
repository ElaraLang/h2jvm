{
  latest = {
    hedgehog = {
      meta = {
        sha256 = "04cjnz4i1qs3v9bza8a3ry1czapwqgxazhywkjzq2rg1544gjmby";
        url = "https://hackage.haskell.org";
        ver = "1.7";
      };
      drv =
        {
          mkDerivation,
          ansi-terminal,
          async,
          barbies,
          base,
          bytestring,
          concurrent-output,
          containers,
          deepseq,
          directory,
          erf,
          exceptions,
          lib,
          lifted-async,
          mmorph,
          monad-control,
          mtl,
          pretty-show,
          primitive,
          random,
          resourcet,
          safe-exceptions,
          stm,
          template-haskell,
          text,
          time,
          transformers,
          transformers-base,
          wl-pprint-annotated,
        }:
        mkDerivation {
          pname = "hedgehog";
          version = "1.7";
          src = /nix/store/piimk6ymh2yg2m74npn5p2znh3wvard4-source;
          libraryHaskellDepends = [
            ansi-terminal
            async
            barbies
            base
            bytestring
            concurrent-output
            containers
            deepseq
            directory
            erf
            exceptions
            lifted-async
            mmorph
            monad-control
            mtl
            pretty-show
            primitive
            random
            resourcet
            safe-exceptions
            stm
            template-haskell
            text
            time
            transformers
            transformers-base
            wl-pprint-annotated
          ];
          testHaskellDepends = [
            base
            containers
            mmorph
            mtl
            pretty-show
            text
            transformers
          ];
          homepage = "http://github.com/hedgehogqa/haskell-hedgehog";
          description = "Release with confidence";
          license = lib.licenses.bsd3;
        };
    };
    opt-env-conf = {
      meta = {
        sha256 = "16cvkxn8gb9pw7j1r0ykfi5d66wp1pik4zg1nb5xp14d7xlc58lf";
        url = "https://hackage.haskell.org";
        ver = "0.15.0.1";
      };
      drv =
        {
          mkDerivation,
          aeson,
          autodocodec,
          autodocodec-nix,
          autodocodec-schema,
          autodocodec-yaml,
          base,
          containers,
          hashable,
          lib,
          mtl,
          path,
          path-io,
          safe-coloured-text,
          safe-coloured-text-layout,
          safe-coloured-text-terminfo,
          selective,
          text,
          validity,
          validity-containers,
          validity-text,
        }:
        mkDerivation {
          pname = "opt-env-conf";
          version = "0.15.0.1";
          src = /nix/store/6awvi6v5k8my01hlx9kz7vg934bd6vnv-source;
          libraryHaskellDepends = [
            aeson
            autodocodec
            autodocodec-nix
            autodocodec-schema
            autodocodec-yaml
            base
            containers
            hashable
            mtl
            path
            path-io
            safe-coloured-text
            safe-coloured-text-layout
            safe-coloured-text-terminfo
            selective
            text
            validity
            validity-containers
            validity-text
          ];
          homepage = "https://github.com/NorfairKing/opt-env-conf#readme";
          description = "Settings parsing for Haskell: command-line arguments, environment variables, and configuration values";
          license = lib.licenses.lgpl3Only;
        };
    };
    sydtest = {
      meta = {
        sha256 = "0l0hbi44cjwic7nczs9nfwywlajg6acr3ja7nr4vz9pa7kw4r38d";
        url = "https://hackage.haskell.org";
        ver = "0.23.0.2";
      };
      drv =
        {
          mkDerivation,
          async,
          autodocodec,
          base,
          bytestring,
          containers,
          deepseq,
          dlist,
          fast-myers-diff,
          filepath,
          lib,
          MonadRandom,
          mtl,
          opt-env-conf,
          path,
          path-io,
          pretty-show,
          QuickCheck,
          quickcheck-io,
          random,
          random-shuffle,
          safe,
          safe-coloured-text,
          safe-coloured-text-terminfo,
          stm,
          svg-builder,
          text,
          vector,
        }:
        mkDerivation {
          pname = "sydtest";
          version = "0.23.0.2";
          src = /nix/store/rrpk3ycldcchi68gkln1j9rrg3s62ac2-source;
          libraryHaskellDepends = [
            async
            autodocodec
            base
            bytestring
            containers
            deepseq
            dlist
            fast-myers-diff
            filepath
            MonadRandom
            mtl
            opt-env-conf
            path
            path-io
            pretty-show
            QuickCheck
            quickcheck-io
            random
            random-shuffle
            safe
            safe-coloured-text
            safe-coloured-text-terminfo
            stm
            svg-builder
            text
            vector
          ];
          homepage = "https://github.com/NorfairKing/sydtest#readme";
          description = "A modern testing framework for Haskell with good defaults and advanced testing features";
          license = "unknown";
        };
    };
    sydtest-hedgehog = {
      meta = {
        sha256 = "06an6p0mhdxm4rhqnhd92f2k248b70fs67xxg74v0v3g7l27iy1s";
        url = "https://hackage.haskell.org";
        ver = "0.4.0.0";
      };
      drv =
        {
          mkDerivation,
          base,
          containers,
          hedgehog,
          lib,
          stm,
          sydtest,
          sydtest-discover,
        }:
        mkDerivation {
          pname = "sydtest-hedgehog";
          version = "0.4.0.0";
          src = /nix/store/6cibwh5j4g0c2chla1k8iysni5wyj91i-source;
          libraryHaskellDepends = [
            base
            containers
            hedgehog
            stm
            sydtest
          ];
          testHaskellDepends = [
            base
            hedgehog
            sydtest
          ];
          testToolDepends = [ sydtest-discover ];
          homepage = "https://github.com/NorfairKing/sydtest#readme";
          description = "A Hedgehog companion library for sydtest";
          license = "unknown";
        };
    };
  };
}
