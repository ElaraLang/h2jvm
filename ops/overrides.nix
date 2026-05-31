{
  latest = {
    hedgehog = {
  meta = {
    sha256 = "04cjnz4i1qs3v9bza8a3ry1czapwqgxazhywkjzq2rg1544gjmby";
    url = "https://hackage.haskell.org";
    ver = "1.7";
  };
  drv = { mkDerivation, ansi-terminal, async, barbies, base, bytestring
, concurrent-output, containers, deepseq, directory, erf
, exceptions, lib, lifted-async, mmorph, monad-control, mtl
, pretty-show, primitive, random, resourcet, safe-exceptions, stm
, template-haskell, text, time, transformers, transformers-base
, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.7";
  src = /nix/store/piimk6ymh2yg2m74npn5p2znh3wvard4-source;
  libraryHaskellDepends = [
    ansi-terminal async barbies base bytestring concurrent-output
    containers deepseq directory erf exceptions lifted-async mmorph
    monad-control mtl pretty-show primitive random resourcet
    safe-exceptions stm template-haskell text time transformers
    transformers-base wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show text transformers
  ];
  homepage = "http://github.com/hedgehogqa/haskell-hedgehog";
  description = "Release with confidence";
  license = lib.licenses.bsd3;
}
;
}
;
    opt-env-conf = {
  meta = {
    sha256 = "16cvkxn8gb9pw7j1r0ykfi5d66wp1pik4zg1nb5xp14d7xlc58lf";
    url = "https://hackage.haskell.org";
    ver = "0.15.0.1";
  };
  drv = { mkDerivation, aeson, autodocodec, autodocodec-nix
, autodocodec-schema, autodocodec-yaml, base, containers, hashable
, lib, mtl, path, path-io, safe-coloured-text
, safe-coloured-text-layout, safe-coloured-text-terminfo, selective
, text, validity, validity-containers, validity-text
}:
mkDerivation {
  pname = "opt-env-conf";
  version = "0.15.0.1";
  src = /nix/store/6awvi6v5k8my01hlx9kz7vg934bd6vnv-source;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-nix autodocodec-schema
    autodocodec-yaml base containers hashable mtl path path-io
    safe-coloured-text safe-coloured-text-layout
    safe-coloured-text-terminfo selective text validity
    validity-containers validity-text
  ];
  homepage = "https://github.com/NorfairKing/opt-env-conf#readme";
  description = "Settings parsing for Haskell: command-line arguments, environment variables, and configuration values";
  license = lib.licenses.lgpl3Only;
}
;
}
;
    sydtest = {
  meta = {
    sha256 = "0l0hbi44cjwic7nczs9nfwywlajg6acr3ja7nr4vz9pa7kw4r38d";
    url = "https://hackage.haskell.org";
    ver = "0.23.0.2";
  };
  drv = { mkDerivation, async, autodocodec, base, bytestring, containers
, deepseq, dlist, fast-myers-diff, filepath, lib, MonadRandom, mtl
, opt-env-conf, path, path-io, pretty-show, QuickCheck
, quickcheck-io, random, random-shuffle, safe, safe-coloured-text
, safe-coloured-text-terminfo, stm, svg-builder, text, vector
}:
mkDerivation {
  pname = "sydtest";
  version = "0.23.0.2";
  src = /nix/store/rrpk3ycldcchi68gkln1j9rrg3s62ac2-source;
  libraryHaskellDepends = [
    async autodocodec base bytestring containers deepseq dlist
    fast-myers-diff filepath MonadRandom mtl opt-env-conf path path-io
    pretty-show QuickCheck quickcheck-io random random-shuffle safe
    safe-coloured-text safe-coloured-text-terminfo stm svg-builder text
    vector
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A modern testing framework for Haskell with good defaults and advanced testing features";
  license = "unknown";
}
;
}
;
    sydtest-hedgehog = {
  meta = {
    sha256 = "06an6p0mhdxm4rhqnhd92f2k248b70fs67xxg74v0v3g7l27iy1s";
    url = "https://hackage.haskell.org";
    ver = "0.4.0.0";
  };
  drv = { mkDerivation, base, containers, hedgehog, lib, stm, sydtest
, sydtest-discover
}:
mkDerivation {
  pname = "sydtest-hedgehog";
  version = "0.4.0.0";
  src = /nix/store/6cibwh5j4g0c2chla1k8iysni5wyj91i-source;
  libraryHaskellDepends = [ base containers hedgehog stm sydtest ];
  testHaskellDepends = [ base hedgehog sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A Hedgehog companion library for sydtest";
  license = "unknown";
}
;
}
;
  };
  lower = {
    adjunctions = {
  meta = {
    sha256 = "0bqp5wmabksajw50bcfhvab3gda9hsp04y5abkp6zfnhmq2v1r2y";
    url = "https://hackage.haskell.org";
    ver = "4.4.4";
  };
  drv = { mkDerivation, base, comonad, containers, distributive, free
, hspec, hspec-discover, lib, mtl, profunctors, semigroupoids
, tagged, transformers
}:
mkDerivation {
  pname = "adjunctions";
  version = "4.4.4";
  src = /nix/store/8pmc9pd47fxp3ym0940pfmqdn8ci8i75-source;
  libraryHaskellDepends = [
    base comonad containers distributive free mtl profunctors
    semigroupoids tagged transformers
  ];
  testHaskellDepends = [ base distributive hspec ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/ekmett/adjunctions/";
  description = "Adjunctions and representable functors";
  license = lib.licenses.bsd2;
}
;
}
;
    aeson = {
  meta = {
    sha256 = "0xmdq5pgp66c2wr3ibsh38br7j5zynk9i8i2hvqp820bxh9hi1cw";
    url = "https://hackage.haskell.org";
    ver = "2.3.0.0";
  };
  drv = { mkDerivation, base, base-compat, base-orphans, base16-bytestring
, bytestring, character-ps, containers, data-fix, deepseq, Diff
, directory, dlist, exceptions, filepath, generic-deriving
, generically, hashable, indexed-traversable, integer-conversion
, integer-logarithms, lib, network-uri, OneTuple, primitive
, QuickCheck, quickcheck-instances, scientific, semialign, strict
, tagged, tasty, tasty-golden, tasty-hunit, tasty-quickcheck
, template-haskell, text, text-iso8601, text-short, th-abstraction
, these, time, time-compat, unordered-containers, uuid-types
, vector, witherable
}:
mkDerivation {
  pname = "aeson";
  version = "2.3.0.0";
  src = /nix/store/1z6q5lrhpqviwcmj7zwpvlq9wnxcid7n-source;
  libraryHaskellDepends = [
    base bytestring character-ps containers data-fix deepseq dlist
    exceptions hashable indexed-traversable integer-conversion
    integer-logarithms network-uri OneTuple primitive QuickCheck
    scientific semialign strict tagged template-haskell text
    text-iso8601 text-short th-abstraction these time time-compat
    unordered-containers uuid-types vector witherable
  ];
  testHaskellDepends = [
    base base-compat base-orphans base16-bytestring bytestring
    containers data-fix Diff directory dlist filepath generic-deriving
    generically hashable indexed-traversable integer-logarithms
    network-uri OneTuple QuickCheck quickcheck-instances scientific
    strict tagged tasty tasty-golden tasty-hunit tasty-quickcheck text
    text-short these time time-compat unordered-containers uuid-types
    vector
  ];
  homepage = "https://github.com/haskell/aeson";
  description = "Fast JSON parsing and encoding";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    async = {
  meta = {
    sha256 = "1731pcifiskq6g1b72p34phx85l65ax7mbjw11310b3zwzk0ldyn";
    url = "https://hackage.haskell.org";
    ver = "2.2.6";
  };
  drv = { mkDerivation, base, hashable, HUnit, lib, stm, test-framework
, test-framework-hunit, unordered-containers
}:
mkDerivation {
  pname = "async";
  version = "2.2.6";
  src = /nix/store/gqjb7z6xhgknsx70z3vqfndrrb5s0igk-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base hashable stm unordered-containers ];
  testHaskellDepends = [
    base HUnit stm test-framework test-framework-hunit
  ];
  homepage = "https://github.com/simonmar/async";
  description = "Run IO operations asynchronously and wait for their results";
  license = lib.licenses.bsd3;
}
;
}
;
    attoparsec = {
  meta = {
    sha256 = "0y9dph5axyvr1bfcvmz6qh50bjcp50m2ljra14960anc6g74a3c8";
    url = "https://hackage.haskell.org";
    ver = "0.14.4";
  };
  drv = { mkDerivation, array, base, bytestring, case-insensitive
, containers, deepseq, directory, filepath, ghc-prim, http-types
, lib, parsec, QuickCheck, quickcheck-unicode, scientific, tasty
, tasty-bench, tasty-quickcheck, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "attoparsec";
  version = "0.14.4";
  src = /nix/store/cy9l5kw9c213v64k3q07lgxaga8yai9b-source;
  libraryHaskellDepends = [
    array base bytestring containers deepseq ghc-prim scientific text
    transformers
  ];
  testHaskellDepends = [
    array base bytestring deepseq QuickCheck quickcheck-unicode
    scientific tasty tasty-quickcheck text transformers vector
  ];
  benchmarkHaskellDepends = [
    array base bytestring case-insensitive containers deepseq directory
    filepath ghc-prim http-types parsec scientific tasty-bench text
    transformers unordered-containers vector
  ];
  doHaddock = false;
  homepage = "https://github.com/bgamari/attoparsec";
  description = "Fast combinator parsing for bytestrings and text";
  license = lib.licenses.bsd3;
}
;
}
;
    autodocodec = {
  meta = {
    sha256 = "0wf3bdgqcn4yi1kbzd655q100vpfsp8idwdsp18r1f9wwgry3laa";
    url = "https://hackage.haskell.org";
    ver = "0.5.0.0";
  };
  drv = { mkDerivation, aeson, base, bytestring, containers, dlist, doctest
, hashable, lib, mtl, scientific, text, time, unordered-containers
, validity, validity-scientific, vector
}:
mkDerivation {
  pname = "autodocodec";
  version = "0.5.0.0";
  src = /nix/store/9fc0prdjs998x0jxzv7fmxmx8lmlyng5-source;
  libraryHaskellDepends = [
    aeson base bytestring containers dlist hashable mtl scientific text
    time unordered-containers validity validity-scientific vector
  ];
  testHaskellDepends = [ base doctest ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Self-documenting encoder and decoder";
  license = lib.licenses.mit;
}
;
}
;
    autodocodec-nix = {
  meta = {
    sha256 = "18prm5vvg8l80vd6arpb0qpg7kv9d8yaw5rnlpcbqq7bmgyl1r1c";
    url = "https://hackage.haskell.org";
    ver = "0.1.0.0";
  };
  drv = { mkDerivation, aeson, autodocodec, base, containers, lib
, scientific, text, unordered-containers, vector
}:
mkDerivation {
  pname = "autodocodec-nix";
  version = "0.1.0.0";
  src = /nix/store/0a86sblr1whmblvfvc2ng54fhcxqggpj-source;
  libraryHaskellDepends = [
    aeson autodocodec base containers scientific text
    unordered-containers vector
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec interpreters for nix";
  license = lib.licenses.mit;
}
;
}
;
    autodocodec-schema = {
  meta = {
    sha256 = "1gg6cm9ly10f2gpgsvaxb4clkg5wl97xrglpypwfa83cfc65ac9p";
    url = "https://hackage.haskell.org";
    ver = "0.2.0.1";
  };
  drv = { mkDerivation, aeson, autodocodec, base, containers, lib, mtl
, scientific, text, unordered-containers, validity, validity-aeson
, validity-containers, validity-text
}:
mkDerivation {
  pname = "autodocodec-schema";
  version = "0.2.0.1";
  src = /nix/store/p3wq8fws5cmfdw9xllavif8r699crmmj-source;
  libraryHaskellDepends = [
    aeson autodocodec base containers mtl scientific text
    unordered-containers validity validity-aeson validity-containers
    validity-text
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec interpreters for JSON Schema";
  license = lib.licenses.mit;
}
;
}
;
    autodocodec-yaml = {
  meta = {
    sha256 = "18frp5yd314nhz1yf35bwjbn6cs56m6w0hgva8kd4zlrg0jabcd4";
    url = "https://hackage.haskell.org";
    ver = "0.4.0.2";
  };
  drv = { mkDerivation, autodocodec, autodocodec-schema, base, bytestring
, containers, lib, path, path-io, safe-coloured-text, scientific
, text, vector, yaml
}:
mkDerivation {
  pname = "autodocodec-yaml";
  version = "0.4.0.2";
  src = /nix/store/47651mwf0kwnbjrs8xm3kcp08gcf015k-source;
  libraryHaskellDepends = [
    autodocodec autodocodec-schema base bytestring containers path
    path-io safe-coloured-text scientific text vector yaml
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec interpreters for yaml";
  license = lib.licenses.mit;
}
;
}
;
    bitvec = {
  meta = {
    sha256 = "15rc25nlf8s6kxw7wfplma6znpc6sh2vmginyb5qdyhjidyzglpg";
    url = "https://hackage.haskell.org";
    ver = "1.1.6.0";
  };
  drv = { mkDerivation, base, bytestring, containers, deepseq, ghc-bignum
, lib, primitive, quickcheck-classes, quickcheck-classes-base
, random, tasty, tasty-bench, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "bitvec";
  version = "1.1.6.0";
  src = /nix/store/zx4x6i04an0x4a9vwkkiv2q9iy35y6ix-source;
  libraryHaskellDepends = [
    base bytestring deepseq ghc-bignum primitive vector
  ];
  testHaskellDepends = [
    base ghc-bignum primitive quickcheck-classes
    quickcheck-classes-base tasty tasty-quickcheck vector
  ];
  benchmarkHaskellDepends = [
    base containers ghc-bignum random tasty tasty-bench vector
  ];
  homepage = "https://github.com/Bodigrim/bitvec";
  description = "Space-efficient bit vectors";
  license = lib.licenses.bsd3;
}
;
}
;
    blaze-builder = {
  meta = {
    sha256 = "17b9bxff50wkmqlbwbnaxwl3bfjax3vk9qchk56ca5xwrvk8nxrd";
    url = "https://hackage.haskell.org";
    ver = "0.4.4.1";
  };
  drv = { mkDerivation, base, bytestring, HUnit, lib, QuickCheck
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, utf8-string
}:
mkDerivation {
  pname = "blaze-builder";
  version = "0.4.4.1";
  src = /nix/store/7bgvyzpnfnxz6xvs7rhqmywk93bdrdll-source;
  libraryHaskellDepends = [ base bytestring text ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text utf8-string
  ];
  homepage = "https://github.com/blaze-builder/blaze-builder";
  description = "Efficient buffered output";
  license = lib.licenses.bsd3;
}
;
}
;
    concurrent-output = {
  meta = {
    sha256 = "1w87rrf337s8wc4z3dkh2mk990003jsk18ry5yawv4465k4yvamw";
    url = "https://hackage.haskell.org";
    ver = "1.10.21";
  };
  drv = { mkDerivation, ansi-terminal, async, base, directory, exceptions
, lib, process, stm, terminal-size, text, transformers, unix
}:
mkDerivation {
  pname = "concurrent-output";
  version = "1.10.21";
  src = /nix/store/kwz3gmjbrzcw4iccsx2d0cyn85klblqy-source;
  libraryHaskellDepends = [
    ansi-terminal async base directory exceptions process stm
    terminal-size text transformers unix
  ];
  description = "Ungarble output from several threads or commands";
  license = lib.licenses.bsd2;
}
;
}
;
    conduit = {
  meta = {
    sha256 = "06hxkbsxa4bgyb8k6apdb94zciczygn33xbm6b5w5y33005xalfx";
    url = "https://hackage.haskell.org";
    ver = "1.3.6.1";
  };
  drv = { mkDerivation, base, bytestring, containers, deepseq, directory
, exceptions, filepath, gauge, hspec, lib, mono-traversable, mtl
, mwc-random, primitive, QuickCheck, resourcet, safe, silently
, split, text, transformers, unix, unliftio, unliftio-core, vector
}:
mkDerivation {
  pname = "conduit";
  version = "1.3.6.1";
  src = /nix/store/zqcxch57ip0j84d499isn1ig8wvg9z6m-source;
  libraryHaskellDepends = [
    base bytestring directory exceptions filepath mono-traversable mtl
    primitive resourcet text transformers unix unliftio-core vector
  ];
  testHaskellDepends = [
    base bytestring containers directory exceptions filepath hspec
    mono-traversable mtl QuickCheck resourcet safe silently split text
    transformers unliftio vector
  ];
  benchmarkHaskellDepends = [
    base containers deepseq gauge hspec mwc-random transformers vector
  ];
  homepage = "http://github.com/snoyberg/conduit";
  description = "Streaming data processing library";
  license = lib.licenses.mit;
}
;
}
;
    constraints = {
  meta = {
    sha256 = "00cjd15kn30qgq541s0g3sd2lnvrdswx3bkafk0bmrg9b0kdb6hg";
    url = "https://hackage.haskell.org";
    ver = "0.14.4";
  };
  drv = { mkDerivation, base, binary, boring, deepseq, hashable, hspec
, hspec-discover, lib, mtl, transformers
}:
mkDerivation {
  pname = "constraints";
  version = "0.14.4";
  src = /nix/store/2k6n5ivkla205m35i77cdwf4dn9vdr2x-source;
  libraryHaskellDepends = [
    base binary boring deepseq hashable mtl transformers
  ];
  testHaskellDepends = [ base hspec ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/ekmett/constraints/";
  description = "Constraint manipulation";
  license = lib.meta.getLicenseFromSpdxId "BSD-2-Clause";
}
;
}
;
    data-fix = {
  meta = {
    sha256 = "0x8r2r8gmdvsclaszg90zn7gla6s8r6salbvgfsp0rscdjzj01ry";
    url = "https://hackage.haskell.org";
    ver = "0.3.4";
  };
  drv = { mkDerivation, base, deepseq, hashable, lib }:
mkDerivation {
  pname = "data-fix";
  version = "0.3.4";
  src = /nix/store/rk6gaw2jpjnd6hqhfwd1kr7c0pb5p370-source;
  libraryHaskellDepends = [ base deepseq hashable ];
  homepage = "https://github.com/spell-music/data-fix";
  description = "Fixpoint data types";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    effectful = {
  meta = {
    sha256 = "0hwx2mna18ydmfqcirakdx6qfgjhnv7hnr510a2ghf6ccs0gwgzn";
    url = "https://hackage.haskell.org";
    ver = "2.2.2.0";
  };
  drv = { mkDerivation, async, base, bytestring, containers, directory
, effectful-core, exceptions, lib, lifted-base, primitive, process
, stm, tasty, tasty-bench, tasty-hunit, text, time, unix, unliftio
}:
mkDerivation {
  pname = "effectful";
  version = "2.2.2.0";
  src = /nix/store/61v4kw59f3hmk0his2jmwp89mih2ny4i-source;
  libraryHaskellDepends = [
    async base bytestring directory effectful-core process stm time
    unliftio
  ];
  testHaskellDepends = [
    base containers effectful-core exceptions lifted-base primitive
    tasty tasty-hunit unliftio
  ];
  benchmarkHaskellDepends = [
    async base tasty-bench text unix unliftio
  ];
  description = "An easy to use, performant extensible effects library";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    effectful-core = {
  meta = {
    sha256 = "0ndifywpjy7zv2ghgz7v78il890n6gzi2fgkc0avr4alh2w9apn8";
    url = "https://hackage.haskell.org";
    ver = "2.2.2.0";
  };
  drv = { mkDerivation, base, containers, exceptions, lib, monad-control
, primitive, transformers-base, unliftio-core
}:
mkDerivation {
  pname = "effectful-core";
  version = "2.2.2.0";
  src = /nix/store/lldn8mbqhy9c3bnddix333xa6jgiglcq-source;
  libraryHaskellDepends = [
    base containers exceptions monad-control primitive
    transformers-base unliftio-core
  ];
  doHaddock = false;
  description = "An easy to use, performant extensible effects library";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    effectful-plugin = {
  meta = {
    sha256 = "1gv38zzay4yplcfsm1dibm3vkiymdpic358kz3d7d8a6ygfr00zv";
    url = "https://hackage.haskell.org";
    ver = "1.1.0.3";
  };
  drv = { mkDerivation, base, containers, effectful-core, ghc, lib }:
mkDerivation {
  pname = "effectful-plugin";
  version = "1.1.0.3";
  src = /nix/store/zm2g3chyi3m27dl8rr4iw1w5w64xpxld-source;
  libraryHaskellDepends = [ base containers effectful-core ghc ];
  testHaskellDepends = [ base effectful-core ];
  description = "A GHC plugin for improving disambiguation of effects";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    effectful-th = {
  meta = {
    sha256 = "162virkpn2fql7l8q9xw74xy4bbg4xb3wyr2wsmys6vxz9mlzh4n";
    url = "https://hackage.haskell.org";
    ver = "1.0.0.2";
  };
  drv = { mkDerivation, base, containers, effectful-core, exceptions, lib
, template-haskell, th-abstraction
}:
mkDerivation {
  pname = "effectful-th";
  version = "1.0.0.2";
  src = /nix/store/mxpwp612p8xqx77bqc7yl60i6yz3fccf-source;
  libraryHaskellDepends = [
    base containers effectful-core exceptions template-haskell
    th-abstraction
  ];
  testHaskellDepends = [ base effectful-core ];
  description = "Template Haskell utilities for the effectful library";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    fast-myers-diff = {
  meta = {
    sha256 = "0csbgddfbf67ghpm1w0416dadl2lyfda4cg72b60whc9l1v6g0k3";
    url = "https://hackage.haskell.org";
    ver = "0.0.1";
  };
  drv = { mkDerivation, base, deepseq, dlist, hspec, lib, text, vector }:
mkDerivation {
  pname = "fast-myers-diff";
  version = "0.0.1";
  src = /nix/store/czjbp8yd5yzcdi8r0agmncf9z2n20wd1-source;
  libraryHaskellDepends = [ base deepseq dlist text vector ];
  testHaskellDepends = [ base hspec text vector ];
  homepage = "https://github.com/NorfairKing/fast-myers-diff#readme";
  description = "A fast implementation of the Myers diff algorithm";
  license = lib.licenses.mit;
}
;
}
;
    free = {
  meta = {
    sha256 = "0b646kh0jwyswi548z1maqjircac4c80zfm0fz06jr0yd0ydrjq1";
    url = "https://hackage.haskell.org";
    ver = "5.2";
  };
  drv = { mkDerivation, base, comonad, containers, distributive, exceptions
, indexed-traversable, lib, mtl, profunctors, semigroupoids
, template-haskell, th-abstraction, transformers, transformers-base
}:
mkDerivation {
  pname = "free";
  version = "5.2";
  src = /nix/store/l46w3zc1q9q9xjhlh3gjdas7lwhinlq0-source;
  libraryHaskellDepends = [
    base comonad containers distributive exceptions indexed-traversable
    mtl profunctors semigroupoids template-haskell th-abstraction
    transformers transformers-base
  ];
  homepage = "http://github.com/ekmett/free/";
  description = "Monads for free";
  license = lib.licenses.bsd3;
}
;
}
;
    happy = {
  meta = {
    sha256 = "11xfm7y0dxb676635xqcfgqr0syq9j3hy1157f3kxpb3ljsyg85a";
    url = "https://hackage.haskell.org";
    ver = "2.2";
  };
  drv = { mkDerivation, base, happy-lib, lib, process }:
mkDerivation {
  pname = "happy";
  version = "2.2";
  src = /nix/store/23x6rn3schxs2r5y1b1235vm9ifg3s11-source;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base happy-lib ];
  testHaskellDepends = [ base process ];
  homepage = "https://www.haskell.org/happy/";
  description = "Happy is a parser generator for Haskell";
  license = lib.licenses.bsd2;
  mainProgram = "happy";
}
;
}
;
    happy-lib = {
  meta = {
    sha256 = "1j83gcfi1w11p9yb87b543lmkbf3xajyfbid7y2mv0s75jsvqgym";
    url = "https://hackage.haskell.org";
    ver = "2.2";
  };
  drv = { mkDerivation, array, base, containers, lib, mtl, transformers }:
mkDerivation {
  pname = "happy-lib";
  version = "2.2";
  src = /nix/store/iwsm64iir2xxinc2lk2sxfhm1j3kq1fc-source;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ array base containers mtl transformers ];
  doHaddock = false;
  homepage = "https://www.haskell.org/happy/";
  description = "Happy is a parser generator for Haskell implemented using this library";
  license = lib.meta.getLicenseFromSpdxId "BSD-2-Clause";
}
;
}
;
    hashable = {
  meta = {
    sha256 = "1zfkla3kjd7b4w5bd93vv71f8gj5849vi924j3kl68cj1njk8i6a";
    url = "https://hackage.haskell.org";
    ver = "1.4.7.0";
  };
  drv = { mkDerivation, base, bytestring, containers, deepseq, filepath
, ghc-bignum, ghc-prim, HUnit, lib, os-string, primitive
, QuickCheck, random, tasty, tasty-hunit, tasty-quickcheck, text
, unix
}:
mkDerivation {
  pname = "hashable";
  version = "1.4.7.0";
  src = /nix/store/c476cyp67qw31g4vhzkasqv2xkz8ybp1-source;
  libraryHaskellDepends = [
    base bytestring containers deepseq filepath ghc-bignum ghc-prim
    os-string text
  ];
  testHaskellDepends = [
    base bytestring filepath ghc-prim HUnit os-string primitive
    QuickCheck random tasty tasty-hunit tasty-quickcheck text unix
  ];
  homepage = "http://github.com/haskell-unordered-containers/hashable";
  description = "A class for types that can be converted to a hash value";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    hedgehog = {
  meta = {
    sha256 = "1qxxhs720im0wpa5lsca0l8qsfmhbyphd1aq01nv96v29lgv795b";
    url = "https://hackage.haskell.org";
    ver = "1.4";
  };
  drv = { mkDerivation, ansi-terminal, async, barbies, base, bytestring
, concurrent-output, containers, deepseq, directory, erf
, exceptions, lib, lifted-async, mmorph, monad-control, mtl
, pretty-show, primitive, random, resourcet, safe-exceptions, stm
, template-haskell, text, time, transformers, transformers-base
, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.4";
  src = /nix/store/h0hfs9fnv1wpvc4x48m9i5p66gx0li8w-source;
  libraryHaskellDepends = [
    ansi-terminal async barbies base bytestring concurrent-output
    containers deepseq directory erf exceptions lifted-async mmorph
    monad-control mtl pretty-show primitive random resourcet
    safe-exceptions stm template-haskell text time transformers
    transformers-base wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Release with confidence";
  license = lib.licenses.bsd3;
}
;
}
;
    indexed-traversable-instances = {
  meta = {
    sha256 = "1issj9yfpxnshm6k7xq3wmmgrhn87cb0jalp0d1ls3zqx0qjrr03";
    url = "https://hackage.haskell.org";
    ver = "0.1.2.1";
  };
  drv = { mkDerivation, base, containers, indexed-traversable, lib
, OneTuple, QuickCheck, quickcheck-instances, tagged, tasty
, tasty-quickcheck, unordered-containers, vector
}:
mkDerivation {
  pname = "indexed-traversable-instances";
  version = "0.1.2.1";
  src = /nix/store/4fg6lbfn2wpy1lfqwyvhm70r92n5k437-source;
  libraryHaskellDepends = [
    base indexed-traversable OneTuple tagged unordered-containers
    vector
  ];
  testHaskellDepends = [
    base containers indexed-traversable OneTuple QuickCheck
    quickcheck-instances tasty tasty-quickcheck unordered-containers
    vector
  ];
  description = "More instances of FunctorWithIndex, FoldableWithIndex, TraversableWithIndex";
  license = lib.licenses.bsd2;
}
;
}
;
    integer-conversion = {
  meta = {
    sha256 = "0jrch63xc80fq6s14zwi5wcmbrj8zr7anl420sq98aglx3df9yr3";
    url = "https://hackage.haskell.org";
    ver = "0.1.1";
  };
  drv = { mkDerivation, base, bytestring, lib, primitive, QuickCheck, tasty
, tasty-bench, tasty-quickcheck, text
}:
mkDerivation {
  pname = "integer-conversion";
  version = "0.1.1";
  src = /nix/store/8h4fhg09lr94h7izdackqaf0hyf8wnz6-source;
  libraryHaskellDepends = [ base bytestring primitive text ];
  testHaskellDepends = [
    base bytestring QuickCheck tasty tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [ base bytestring tasty-bench text ];
  homepage = "https://github.com/phadej/integer-conversion";
  description = "Conversion from strings to Integer";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    invariant = {
  meta = {
    sha256 = "1arihzidi3jkn26l01mgql4dk3iqm5rl6ns4swr79vqi8i3k4qkx";
    url = "https://hackage.haskell.org";
    ver = "0.6.5";
  };
  drv = { mkDerivation, array, base, bifunctors, comonad, containers
, contravariant, hspec, hspec-discover, lib, profunctors
, QuickCheck, StateVar, stm, tagged, template-haskell
, th-abstraction, transformers, unordered-containers
}:
mkDerivation {
  pname = "invariant";
  version = "0.6.5";
  src = /nix/store/9b9pldp2xq5gb36bixw78mgdd5yk9gzd-source;
  libraryHaskellDepends = [
    array base bifunctors comonad containers contravariant profunctors
    StateVar stm tagged template-haskell th-abstraction transformers
    unordered-containers
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/nfrisby/invariant-functors";
  description = "Haskell98 invariant functors";
  license = lib.licenses.bsd2;
}
;
}
;
    kan-extensions = {
  meta = {
    sha256 = "002j5356ls1gcik2rrmjg10vk1p6g6n0hjf7h1x96zab1k4z21bc";
    url = "https://hackage.haskell.org";
    ver = "5.2.8";
  };
  drv = { mkDerivation, adjunctions, base, comonad, contravariant
, distributive, exceptions, free, invariant, lib, mtl, profunctors
, semigroupoids, transformers
}:
mkDerivation {
  pname = "kan-extensions";
  version = "5.2.8";
  src = /nix/store/g9mmclp00v99ygy2fj03zz08bmks5hnk-source;
  libraryHaskellDepends = [
    adjunctions base comonad contravariant distributive exceptions free
    invariant mtl profunctors semigroupoids transformers
  ];
  homepage = "http://github.com/ekmett/kan-extensions/";
  description = "Kan extensions, Kan lifts, the Yoneda lemma, and (co)density (co)monads";
  license = lib.licenses.bsd3;
}
;
}
;
    lens = {
  meta = {
    sha256 = "0y6f4nflzbyk93xzcc0w842ya7b4waa1llj07cqaq5cy5bwsr4ia";
    url = "https://hackage.haskell.org";
    ver = "5.3";
  };
  drv = { mkDerivation, array, assoc, base, base-orphans, bifunctors
, bytestring, call-stack, comonad, containers, contravariant
, criterion, deepseq, distributive, exceptions, filepath, free
, generic-deriving, ghc-prim, hashable, HUnit, indexed-traversable
, indexed-traversable-instances, kan-extensions, lib, mtl, parallel
, profunctors, QuickCheck, reflection, semigroupoids
, simple-reflect, strict, tagged, template-haskell, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
, th-abstraction, these, transformers, transformers-compat
, unordered-containers, vector
}:
mkDerivation {
  pname = "lens";
  version = "5.3";
  src = /nix/store/jscjmnzm6brk48vsdlslvs65l6lrqfld-source;
  libraryHaskellDepends = [
    array assoc base base-orphans bifunctors bytestring call-stack
    comonad containers contravariant distributive exceptions filepath
    free ghc-prim hashable indexed-traversable
    indexed-traversable-instances kan-extensions mtl parallel
    profunctors reflection semigroupoids strict tagged template-haskell
    text th-abstraction these transformers transformers-compat
    unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring containers deepseq HUnit mtl QuickCheck
    simple-reflect test-framework test-framework-hunit
    test-framework-quickcheck2 text transformers
  ];
  benchmarkHaskellDepends = [
    base bytestring comonad containers criterion deepseq
    generic-deriving transformers unordered-containers vector
  ];
  homepage = "http://github.com/ekmett/lens/";
  description = "Lenses, Folds and Traversals";
  license = lib.licenses.bsd2;
}
;
}
;
    libyaml = {
  meta = {
    sha256 = "0ng8mjm30bd3jl1jyc5zm6vmz1xjd11dn48vx2kncmihm9g3fvzm";
    url = "https://hackage.haskell.org";
    ver = "0.1.4";
  };
  drv = { mkDerivation, base, bytestring, conduit, lib, libyaml-clib
, resourcet
}:
mkDerivation {
  pname = "libyaml";
  version = "0.1.4";
  src = /nix/store/vawxc75r0cr3x7gyj17mjarh6bj14rq3-source;
  libraryHaskellDepends = [
    base bytestring conduit libyaml-clib resourcet
  ];
  homepage = "https://github.com/snoyberg/yaml#readme";
  description = "Low-level, streaming YAML interface";
  license = lib.licenses.bsd3;
}
;
}
;
    lifted-async = {
  meta = {
    sha256 = "0cgzs8sfr3l7ah5nnscpp50v5mmvc4hqf02zdi4h344dbbha10fy";
    url = "https://hackage.haskell.org";
    ver = "0.10.2.7";
  };
  drv = { mkDerivation, async, base, constraints, deepseq, HUnit, lib
, lifted-base, monad-control, mtl, tasty, tasty-bench
, tasty-expected-failure, tasty-hunit, tasty-th, transformers-base
}:
mkDerivation {
  pname = "lifted-async";
  version = "0.10.2.7";
  src = /nix/store/7fr6j14aj5sb57yg621rc9vysc7d1qcz-source;
  libraryHaskellDepends = [
    async base constraints lifted-base monad-control transformers-base
  ];
  testHaskellDepends = [
    async base HUnit lifted-base monad-control mtl tasty
    tasty-expected-failure tasty-hunit tasty-th
  ];
  benchmarkHaskellDepends = [ async base deepseq tasty-bench ];
  homepage = "https://github.com/maoe/lifted-async";
  description = "Run lifted IO operations asynchronously and wait for their results";
  license = lib.licenses.bsd3;
}
;
}
;
    mono-traversable = {
  meta = {
    sha256 = "1si42kg8b1ic77wbnkzy2x1jlb8gmpxkbnqf8xi2697i00mni3x8";
    url = "https://hackage.haskell.org";
    ver = "1.0.21.0";
  };
  drv = { mkDerivation, base, bytestring, containers, deepseq, foldl, gauge
, hashable, hspec, hspec-discover, HUnit, lib, mwc-random
, QuickCheck, split, text, transformers, unordered-containers
, vector, vector-algorithms
}:
mkDerivation {
  pname = "mono-traversable";
  version = "1.0.21.0";
  src = /nix/store/syb8cw0a2cg0sbi2qisqx7aix0mbbidd-source;
  libraryHaskellDepends = [
    base bytestring containers hashable split text transformers
    unordered-containers vector vector-algorithms
  ];
  testHaskellDepends = [
    base bytestring containers foldl hspec HUnit QuickCheck text
    transformers unordered-containers vector
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    base bytestring containers deepseq gauge mwc-random text vector
  ];
  homepage = "https://github.com/snoyberg/mono-traversable#readme";
  description = "Type classes for mapping, folding, and traversing monomorphic containers";
  license = lib.licenses.mit;
}
;
}
;
    network-uri = {
  meta = {
    sha256 = "0zj83viziy80f7nybpmc1hki8wrd8pzps31fxns9vxhc1p7l9chj";
    url = "https://hackage.haskell.org";
    ver = "2.6.4.2";
  };
  drv = { mkDerivation, base, criterion, deepseq, HUnit, lib, parsec
, QuickCheck, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, th-compat
}:
mkDerivation {
  pname = "network-uri";
  version = "2.6.4.2";
  src = /nix/store/7rvxjdh21n002q701i7lrx33c3z2y5dl-source;
  libraryHaskellDepends = [
    base deepseq parsec template-haskell th-compat
  ];
  testHaskellDepends = [
    base HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base criterion deepseq HUnit ];
  homepage = "https://github.com/haskell/network-uri";
  description = "URI manipulation";
  license = lib.licenses.bsd3;
}
;
}
;
    opt-env-conf = {
  meta = {
    sha256 = "16cvkxn8gb9pw7j1r0ykfi5d66wp1pik4zg1nb5xp14d7xlc58lf";
    url = "https://hackage.haskell.org";
    ver = "0.15.0.1";
  };
  drv = { mkDerivation, aeson, autodocodec, autodocodec-nix
, autodocodec-schema, autodocodec-yaml, base, containers, hashable
, lib, mtl, path, path-io, safe-coloured-text
, safe-coloured-text-layout, safe-coloured-text-terminfo, selective
, text, validity, validity-containers, validity-text
}:
mkDerivation {
  pname = "opt-env-conf";
  version = "0.15.0.1";
  src = /nix/store/6awvi6v5k8my01hlx9kz7vg934bd6vnv-source;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-nix autodocodec-schema
    autodocodec-yaml base containers hashable mtl path path-io
    safe-coloured-text safe-coloured-text-layout
    safe-coloured-text-terminfo selective text validity
    validity-containers validity-text
  ];
  homepage = "https://github.com/NorfairKing/opt-env-conf#readme";
  description = "Settings parsing for Haskell: command-line arguments, environment variables, and configuration values";
  license = lib.licenses.lgpl3Only;
}
;
}
;
    parsec = {
  meta = {
    sha256 = "089j939xxi6w6a2ggr40c4s2kdbwkzap2mnhvimmf45hg865h48n";
    url = "https://hackage.haskell.org";
    ver = "3.1.18.0";
  };
  drv = { mkDerivation, base, bytestring, deepseq, lib, mtl, tasty
, tasty-hunit, text
}:
mkDerivation {
  pname = "parsec";
  version = "3.1.18.0";
  src = /nix/store/js4bapxi3l3jskjy1mm4fr21rllvymxh-source;
  libraryHaskellDepends = [ base bytestring mtl text ];
  testHaskellDepends = [ base deepseq mtl tasty tasty-hunit ];
  homepage = "https://github.com/haskell/parsec";
  description = "Monadic parser combinators";
  license = lib.licenses.bsd2;
}
;
}
;
    path = {
  meta = {
    sha256 = "16hgrkvd27c9vp5447d1dv3b3fi0fv0jfig10h2j37mzk4850wg8";
    url = "https://hackage.haskell.org";
    ver = "0.9.6";
  };
  drv = { mkDerivation, aeson, base, bytestring, deepseq, exceptions
, filepath, genvalidity, genvalidity-hspec, hashable, hspec, lib
, QuickCheck, template-haskell, text, validity-bytestring
}:
mkDerivation {
  pname = "path";
  version = "0.9.6";
  src = /nix/store/17x0d7bdy3wg6nq9zw20ndi417gy13ck-source;
  libraryHaskellDepends = [
    aeson base deepseq exceptions filepath hashable template-haskell
    text
  ];
  testHaskellDepends = [
    aeson base bytestring exceptions filepath genvalidity
    genvalidity-hspec hspec QuickCheck template-haskell
    validity-bytestring
  ];
  doHaddock = false;
  description = "Support for well-typed paths";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    path-io = {
  meta = {
    sha256 = "063ma7gzqr5c6s8a1yv72jgll3xdajvgclbc8w0ddmqgcrb62x2k";
    url = "https://hackage.haskell.org";
    ver = "1.8.2";
  };
  drv = { mkDerivation, base, containers, directory, dlist, exceptions
, filepath, hspec, lib, path, temporary, time, transformers
, unix-compat
}:
mkDerivation {
  pname = "path-io";
  version = "1.8.2";
  src = /nix/store/y2n6qszdsqdfhhbw4fl146qzyj1sa7zb-source;
  libraryHaskellDepends = [
    base containers directory dlist exceptions filepath path temporary
    time transformers unix-compat
  ];
  testHaskellDepends = [ base exceptions hspec path unix-compat ];
  homepage = "https://github.com/mrkkrp/path-io";
  description = "Interface to ‘directory’ package for users of ‘path’";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    pretty-show = {
  meta = {
    sha256 = "1q3pkp0ly221yf2r3skr6v0664bb0a6z7x82hvy6yl02ds2g9b1n";
    url = "https://hackage.haskell.org";
    ver = "1.10";
  };
  drv = { mkDerivation, array, base, filepath, ghc-prim, happy
, haskell-lexer, lib, pretty, text
}:
mkDerivation {
  pname = "pretty-show";
  version = "1.10";
  src = /nix/store/hk74slj8bkqv81b7pa18lp5hfzim2f3b-source;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base filepath ghc-prim haskell-lexer pretty text
  ];
  libraryToolDepends = [ happy ];
  executableHaskellDepends = [ base ];
  homepage = "http://wiki.github.com/yav/pretty-show";
  description = "Tools for working with derived `Show` instances and generic inspection of values";
  license = lib.licenses.mit;
  mainProgram = "ppsh";
}
;
}
;
    prettyprinter = {
  meta = {
    sha256 = "17byy08brwcsl5rqdhibq3pcpgx085shizb2ap6s4xy3izdia3cc";
    url = "https://hackage.haskell.org";
    ver = "1.7.0";
  };
  drv = { mkDerivation, ansi-wl-pprint, base, base-compat, bytestring
, containers, deepseq, doctest, gauge, lib, mtl, pgp-wordlist
, QuickCheck, quickcheck-instances, random, tasty, tasty-hunit
, tasty-quickcheck, text, transformers
}:
mkDerivation {
  pname = "prettyprinter";
  version = "1.7.0";
  src = /nix/store/0af89y4b6453gblv4k1j1vw3n03bc3y3-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [
    base bytestring doctest pgp-wordlist QuickCheck
    quickcheck-instances tasty tasty-hunit tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    ansi-wl-pprint base base-compat containers deepseq gauge mtl
    QuickCheck random text transformers
  ];
  homepage = "http://github.com/quchen/prettyprinter";
  description = "A modern, easy to use, well-documented, extensible pretty-printer";
  license = lib.licenses.bsd2;
}
;
}
;
    safe-coloured-text = {
  meta = {
    sha256 = "12k8zx42gdrjlaxxxcjmac52gk2kxxsz24i75ngvy1y7b20q8k8z";
    url = "https://hackage.haskell.org";
    ver = "0.4.0.0";
  };
  drv = { mkDerivation, base, bytestring, lib, text, validity
, validity-bytestring, validity-text
}:
mkDerivation {
  pname = "safe-coloured-text";
  version = "0.4.0.0";
  src = /nix/store/wzyjld4p98qgs2z8sy1y3khwpfkq0d3j-source;
  libraryHaskellDepends = [
    base bytestring text validity validity-bytestring validity-text
  ];
  homepage = "https://github.com/NorfairKing/safe-coloured-text#readme";
  description = "Safely output coloured text";
  license = lib.licenses.mit;
}
;
}
;
    safe-coloured-text-layout = {
  meta = {
    sha256 = "0n2dcd8algzlrlw2vmwjhbpqqm3r2px9g4xjjm9yr03vjqkw3952";
    url = "https://hackage.haskell.org";
    ver = "0.2.0.1";
  };
  drv = { mkDerivation, base, lib, safe-coloured-text, text, validity }:
mkDerivation {
  pname = "safe-coloured-text-layout";
  version = "0.2.0.1";
  src = /nix/store/ska1cj37kfzj1zk5sig6hwj23mss8nsw-source;
  libraryHaskellDepends = [ base safe-coloured-text text validity ];
  homepage = "https://github.com/NorfairKing/safe-coloured-text#readme";
  description = "Safely layout output coloured text";
  license = lib.licenses.mit;
}
;
}
;
    safe-coloured-text-terminfo = {
  meta = {
    sha256 = "0r569a6f41w07i68js02755kiaakcnsgkn6k791iz9l2q67gs5wj";
    url = "https://hackage.haskell.org";
    ver = "0.3.0.0";
  };
  drv = { mkDerivation, base, lib, safe-coloured-text, terminfo }:
mkDerivation {
  pname = "safe-coloured-text-terminfo";
  version = "0.3.0.0";
  src = /nix/store/p0xvapjsm0qs1f5brsil01a30jgw2akl-source;
  libraryHaskellDepends = [ base safe-coloured-text terminfo ];
  homepage = "https://github.com/NorfairKing/safe-coloured-text#readme";
  description = "Safely output coloured text";
  license = lib.licenses.mit;
}
;
}
;
    scientific = {
  meta = {
    sha256 = "0imbwigr1m378bk51gc2d8cbrj5r8sdv3bgvn0386lc07sayp3ng";
    url = "https://hackage.haskell.org";
    ver = "0.3.8.1";
  };
  drv = { mkDerivation, base, binary, bytestring, containers, criterion
, deepseq, hashable, integer-logarithms, lib, primitive, QuickCheck
, smallcheck, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, template-haskell, text
}:
mkDerivation {
  pname = "scientific";
  version = "0.3.8.1";
  src = /nix/store/7hfb4zppkr05zrfhsimw6mrjfq5hmwaa-source;
  libraryHaskellDepends = [
    base binary bytestring containers deepseq hashable
    integer-logarithms primitive template-haskell text
  ];
  testHaskellDepends = [
    base binary bytestring QuickCheck smallcheck tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck text
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/basvandijk/scientific";
  description = "Numbers represented using scientific notation";
  license = lib.licenses.bsd3;
}
;
}
;
    semialign = {
  meta = {
    sha256 = "1hanj5gkmk9sbzbx6zbk4xin9s4vgk1zidk100kvg20xsgfpkwi0";
    url = "https://hackage.haskell.org";
    ver = "1.4";
  };
  drv = { mkDerivation, base, containers, hashable, indexed-traversable
, indexed-traversable-instances, lib, semigroupoids, tagged, these
, unordered-containers, vector
}:
mkDerivation {
  pname = "semialign";
  version = "1.4";
  src = /nix/store/v0mn1cf4kl6p9zmsb4g29qnzq8ggbzlz-source;
  libraryHaskellDepends = [
    base containers hashable indexed-traversable
    indexed-traversable-instances semigroupoids tagged these
    unordered-containers vector
  ];
  homepage = "https://github.com/haskellari/these";
  description = "Align and Zip type-classes from the common Semialign ancestor";
  license = lib.licenses.bsd3;
}
;
}
;
    semigroupoids = {
  meta = {
    sha256 = "0nc2c573inxnp4nz3pbahb66ca9750zdgashwnak7kxyrq7d763l";
    url = "https://hackage.haskell.org";
    ver = "6.0.2";
  };
  drv = { mkDerivation, base, base-orphans, bifunctors, comonad, containers
, contravariant, hashable, lib, tagged, template-haskell
, transformers, transformers-compat, unordered-containers
}:
mkDerivation {
  pname = "semigroupoids";
  version = "6.0.2";
  src = /nix/store/clbl4jx9x8bnjickxhp9s0k5hc87rfq4-source;
  libraryHaskellDepends = [
    base base-orphans bifunctors comonad containers contravariant
    hashable tagged template-haskell transformers transformers-compat
    unordered-containers
  ];
  homepage = "http://github.com/ekmett/semigroupoids";
  description = "Semigroupoids: Category sans id";
  license = lib.licenses.bsd2;
}
;
}
;
    strict = {
  meta = {
    sha256 = "06y3ab0nsdbrkrxzc7hgy6cwxl72wcgqn52bs1vvi5lkp64v559y";
    url = "https://hackage.haskell.org";
    ver = "0.5.1";
  };
  drv = { mkDerivation, assoc, base, binary, bytestring, deepseq, ghc-prim
, hashable, lib, text, these, transformers
}:
mkDerivation {
  pname = "strict";
  version = "0.5.1";
  src = /nix/store/p7v6sdqgj45jfxfcyl5cg48b4sj6snki-source;
  libraryHaskellDepends = [
    assoc base binary bytestring deepseq ghc-prim hashable text these
    transformers
  ];
  homepage = "https://github.com/haskell-strict/strict";
  description = "Strict data types and String IO";
  license = lib.licenses.bsd3;
}
;
}
;
    svg-builder = {
  meta = {
    sha256 = "1r2d06kafz2ahhbj1sip9liqv04zcmp1rciwx062m6880zm3pklm";
    url = "https://hackage.haskell.org";
    ver = "0.1.1";
  };
  drv = { mkDerivation, base, blaze-builder, bytestring, hashable, lib
, text, unordered-containers
}:
mkDerivation {
  pname = "svg-builder";
  version = "0.1.1";
  src = /nix/store/mar425nrnr7mphrnvdbxyasrz4x0a9ni-source;
  libraryHaskellDepends = [
    base blaze-builder bytestring hashable text unordered-containers
  ];
  homepage = "https://github.com/diagrams/svg-builder.git";
  description = "DSL for building SVG";
  license = lib.licenses.bsd3;
}
;
}
;
    sydtest = {
  meta = {
    sha256 = "1g2w1sn1bi22s7vbb9ss9kgi66jplsc122mvsnjsn492g0xjxwkb";
    url = "https://hackage.haskell.org";
    ver = "0.16.0.0";
  };
  drv = { mkDerivation, async, autodocodec, base, bytestring, containers
, dlist, fast-myers-diff, filepath, lib, MonadRandom, mtl
, opt-env-conf, path, path-io, pretty-show, QuickCheck
, quickcheck-io, random, random-shuffle, safe, safe-coloured-text
, safe-coloured-text-terminfo, stm, svg-builder, text, vector
}:
mkDerivation {
  pname = "sydtest";
  version = "0.16.0.0";
  src = /nix/store/1sfh3rx1f47xbbb8yj3qjhna49hm6mz8-source;
  libraryHaskellDepends = [
    async autodocodec base bytestring containers dlist fast-myers-diff
    filepath MonadRandom mtl opt-env-conf path path-io pretty-show
    QuickCheck quickcheck-io random random-shuffle safe
    safe-coloured-text safe-coloured-text-terminfo stm svg-builder text
    vector
  ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A modern testing framework for Haskell with good defaults and advanced testing features";
  license = "unknown";
}
;
}
;
    sydtest-hedgehog = {
  meta = {
    sha256 = "06an6p0mhdxm4rhqnhd92f2k248b70fs67xxg74v0v3g7l27iy1s";
    url = "https://hackage.haskell.org";
    ver = "0.4.0.0";
  };
  drv = { mkDerivation, base, containers, hedgehog, lib, stm, sydtest
, sydtest-discover
}:
mkDerivation {
  pname = "sydtest-hedgehog";
  version = "0.4.0.0";
  src = /nix/store/6cibwh5j4g0c2chla1k8iysni5wyj91i-source;
  libraryHaskellDepends = [ base containers hedgehog stm sydtest ];
  testHaskellDepends = [ base hedgehog sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/sydtest#readme";
  description = "A Hedgehog companion library for sydtest";
  license = "unknown";
}
;
}
;
    text = {
  meta = {
    sha256 = "1gi9f9karjfl577bmkgd5ldygq68f54nfw8hwpqlsf0b5n4f14s8";
    url = "https://hackage.haskell.org";
    ver = "2.0.2";
  };
  drv = { mkDerivation, array, base, binary, bytestring, containers
, deepseq, directory, filepath, ghc-prim, lib, QuickCheck
, system-cxx-std-lib, tasty, tasty-bench, tasty-hunit
, tasty-inspection-testing, tasty-quickcheck, template-haskell
, transformers
}:
mkDerivation {
  pname = "text";
  version = "2.0.2";
  src = /nix/store/n5p9245l32398b82shllff630fhkmw9v-source;
  libraryHaskellDepends = [
    array base binary bytestring deepseq ghc-prim system-cxx-std-lib
    template-haskell
  ];
  testHaskellDepends = [
    base bytestring deepseq directory ghc-prim QuickCheck tasty
    tasty-hunit tasty-inspection-testing tasty-quickcheck
    template-haskell transformers
  ];
  benchmarkHaskellDepends = [
    base bytestring containers deepseq directory filepath tasty-bench
    transformers
  ];
  doCheck = false;
  homepage = "https://github.com/haskell/text";
  description = "An efficient packed Unicode text type";
  license = lib.meta.getLicenseFromSpdxId "BSD-2-Clause";
}
;
}
;
    text-iso8601 = {
  meta = {
    sha256 = "0qh0lgfd0rav0wa93chi983jyhdqzalrj5ywrvv65fnig111nv0h";
    url = "https://hackage.haskell.org";
    ver = "0.2.0.0";
  };
  drv = { mkDerivation, attoparsec, attoparsec-iso8601, base
, integer-conversion, lib, QuickCheck, quickcheck-instances, tasty
, tasty-bench, tasty-hunit, tasty-quickcheck, text, time
, time-compat
}:
mkDerivation {
  pname = "text-iso8601";
  version = "0.2.0.0";
  src = /nix/store/9bm8nrk1gi4sl187jgbncq7048h74miy-source;
  libraryHaskellDepends = [
    base integer-conversion text time time-compat
  ];
  testHaskellDepends = [
    base QuickCheck quickcheck-instances tasty tasty-hunit
    tasty-quickcheck text time-compat
  ];
  benchmarkHaskellDepends = [
    attoparsec attoparsec-iso8601 base tasty-bench text
  ];
  homepage = "https://github.com/haskell/aeson";
  description = "Converting time to and from ISO 8601 text";
  license = lib.licenses.bsd3;
}
;
}
;
    text-short = {
  meta = {
    sha256 = "1yzyzklry9cdc12283b0zf0kpa8nb7gixmdaf3l8x7388zpxhhay";
    url = "https://hackage.haskell.org";
    ver = "0.1.6.1";
  };
  drv = { mkDerivation, base, binary, bytestring, deepseq, ghc-prim
, hashable, lib, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text
}:
mkDerivation {
  pname = "text-short";
  version = "0.1.6.1";
  src = /nix/store/bf8cszj81rj7svdscshl17z7mnr8zrdk-source;
  libraryHaskellDepends = [
    base binary bytestring deepseq ghc-prim hashable template-haskell
    text
  ];
  testHaskellDepends = [
    base binary bytestring tasty tasty-hunit tasty-quickcheck text
  ];
  description = "Memory-efficient representation of Unicode text strings";
  license = lib.licenses.bsd3;
}
;
}
;
    these = {
  meta = {
    sha256 = "0jqchlmycfcvkff48shhkswansnzrw57q8945m483mrd59zpg27k";
    url = "https://hackage.haskell.org";
    ver = "1.2.1";
  };
  drv = { mkDerivation, assoc, base, binary, deepseq, hashable, lib }:
mkDerivation {
  pname = "these";
  version = "1.2.1";
  src = /nix/store/aaw05vz42pjyhry145973mssbqw1n5i9-source;
  libraryHaskellDepends = [ assoc base binary deepseq hashable ];
  homepage = "https://github.com/haskellari/these";
  description = "An either-or-both data type";
  license = lib.licenses.bsd3;
}
;
}
;
    time-compat = {
  meta = {
    sha256 = "02yq6qc9fbawpxkypaf4nm9vidfv5vvgidxyj4r3dxa4lb29jd2p";
    url = "https://hackage.haskell.org";
    ver = "1.9.9";
  };
  drv = { mkDerivation, base, base-orphans, deepseq, hashable, HUnit, lib
, QuickCheck, random, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, time
}:
mkDerivation {
  pname = "time-compat";
  version = "1.9.9";
  src = /nix/store/5d4j6ha2hgp5qfaw2li1gwh8wbn8y7xq-source;
  libraryHaskellDepends = [
    base base-orphans deepseq hashable template-haskell time
  ];
  testHaskellDepends = [
    base deepseq hashable HUnit QuickCheck random tasty tasty-hunit
    tasty-quickcheck template-haskell
  ];
  homepage = "https://github.com/haskellari/time-compat";
  description = "Compatibility package for time";
  license = lib.licenses.bsd3;
}
;
}
;
    unliftio = {
  meta = {
    sha256 = "0cp92d9f2hzya636y7w8m0gw7ik6ri2clzpdnz5klh917nnbd7ii";
    url = "https://hackage.haskell.org";
    ver = "0.2.25.1";
  };
  drv = { mkDerivation, async, base, bytestring, containers, deepseq
, directory, filepath, gauge, hspec, lib, process, QuickCheck
, safe-exceptions, stm, time, transformers, unix, unliftio-core
}:
mkDerivation {
  pname = "unliftio";
  version = "0.2.25.1";
  src = /nix/store/ijkdj9swchdhsz5dg7vsvzwpfh5kinfc-source;
  libraryHaskellDepends = [
    async base bytestring deepseq directory filepath process
    safe-exceptions stm time transformers unix unliftio-core
  ];
  testHaskellDepends = [
    async base bytestring containers deepseq directory filepath hspec
    process QuickCheck safe-exceptions stm time transformers unix
    unliftio-core
  ];
  benchmarkHaskellDepends = [
    async base bytestring deepseq directory filepath gauge process
    safe-exceptions stm time transformers unix unliftio-core
  ];
  homepage = "https://github.com/fpco/unliftio/tree/master/unliftio#readme";
  description = "The MonadUnliftIO typeclass for unlifting monads to IO (batteries included)";
  license = lib.licenses.mit;
}
;
}
;
    unordered-containers = {
  meta = {
    sha256 = "0na84q5vxxww3pmz72ihpx4j7dhk71z28r55i7j0pq7mj27jasb0";
    url = "https://hackage.haskell.org";
    ver = "0.2.21";
  };
  drv = { mkDerivation, base, bytestring, ChasingBottoms, containers
, deepseq, hashable, hashmap, HUnit, lib, nothunks, QuickCheck
, random, tasty, tasty-bench, tasty-hunit, tasty-quickcheck
, template-haskell
}:
mkDerivation {
  pname = "unordered-containers";
  version = "0.2.21";
  src = /nix/store/ld4hwdryaajryhzbsrflbpnqvd0pj634-source;
  libraryHaskellDepends = [ base deepseq hashable template-haskell ];
  testHaskellDepends = [
    base ChasingBottoms containers hashable HUnit nothunks QuickCheck
    random tasty tasty-hunit tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base bytestring containers deepseq hashable hashmap random
    tasty-bench
  ];
  homepage = "https://github.com/haskell-unordered-containers/unordered-containers";
  description = "Efficient hashing-based container types";
  license = lib.licenses.bsd3;
}
;
}
;
    uuid-types = {
  meta = {
    sha256 = "1jrid43smmfcchrfwpzkxil16a4c5016y4b49yjka0sildj1lprg";
    url = "https://hackage.haskell.org";
    ver = "1.0.6.1";
  };
  drv = { mkDerivation, base, binary, bytestring, deepseq, hashable, lib
, QuickCheck, random, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text
}:
mkDerivation {
  pname = "uuid-types";
  version = "1.0.6.1";
  src = /nix/store/d7pn428v517nab28kznyyr4ccypibj48-source;
  libraryHaskellDepends = [
    base binary bytestring deepseq hashable random template-haskell
    text
  ];
  testHaskellDepends = [
    base binary bytestring QuickCheck tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/haskell-hvr/uuid";
  description = "Type definitions for Universally Unique Identifiers";
  license = lib.licenses.bsd3;
}
;
}
;
    validity-aeson = {
  meta = {
    sha256 = "0wirg6wjzpc37073dia3c82i2svihn2bbh31kxwqxjl6zqw5a1ia";
    url = "https://hackage.haskell.org";
    ver = "0.2.0.5";
  };
  drv = { mkDerivation, aeson, base, hspec, lib, validity
, validity-scientific, validity-text, validity-unordered-containers
, validity-vector
}:
mkDerivation {
  pname = "validity-aeson";
  version = "0.2.0.5";
  src = /nix/store/336n9008ki42b03s2m469acy93v2w1ri-source;
  libraryHaskellDepends = [
    aeson base validity validity-scientific validity-text
    validity-unordered-containers validity-vector
  ];
  testHaskellDepends = [ aeson base hspec validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for aeson";
  license = lib.licenses.mit;
}
;
}
;
    validity-scientific = {
  meta = {
    sha256 = "1mf2cj90a9y2vbhmsdqfni8by1a3ngzjr4yg3fafq13vhnsylg3m";
    url = "https://hackage.haskell.org";
    ver = "0.2.0.3";
  };
  drv = { mkDerivation, base, lib, scientific, validity }:
mkDerivation {
  pname = "validity-scientific";
  version = "0.2.0.3";
  src = /nix/store/3ll1fjkr9x4ap8df56cfcbhbv04yd0my-source;
  libraryHaskellDepends = [ base scientific validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for scientific";
  license = lib.licenses.mit;
}
;
}
;
    validity-text = {
  meta = {
    sha256 = "0mmlyzfz95j0s81qj7l42vqdl1gdnaa98z90cks8jp4kjxzd6ymp";
    url = "https://hackage.haskell.org";
    ver = "0.3.1.3";
  };
  drv = { mkDerivation, base, bytestring, lib, text, validity }:
mkDerivation {
  pname = "validity-text";
  version = "0.3.1.3";
  src = /nix/store/xg4wm4c51bws4jhq2kibzgp4i2b6wyj5-source;
  libraryHaskellDepends = [ base bytestring text validity ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for text";
  license = lib.licenses.mit;
}
;
}
;
    validity-unordered-containers = {
  meta = {
    sha256 = "01zbbdq3c042w7qwh246nyvd4wmr1f816l1mzfvh1ji35892w7qh";
    url = "https://hackage.haskell.org";
    ver = "0.2.0.3";
  };
  drv = { mkDerivation, base, hashable, lib, unordered-containers, validity
}:
mkDerivation {
  pname = "validity-unordered-containers";
  version = "0.2.0.3";
  src = /nix/store/v7mvjcbq0wyqhs78g2g04aijrawp3k1n-source;
  libraryHaskellDepends = [
    base hashable unordered-containers validity
  ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for unordered-containers";
  license = lib.licenses.mit;
}
;
}
;
    validity-vector = {
  meta = {
    sha256 = "09bf0k76v7iyj7q1sn8kzn1xgdf1f8zfrhnss2mddm42yb8g114p";
    url = "https://hackage.haskell.org";
    ver = "0.2.0.3";
  };
  drv = { mkDerivation, base, hashable, lib, validity, vector }:
mkDerivation {
  pname = "validity-vector";
  version = "0.2.0.3";
  src = /nix/store/nl0c04k549gqffcfl5h23nyibln40h7v-source;
  libraryHaskellDepends = [ base hashable validity vector ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Validity instances for vector";
  license = lib.licenses.mit;
}
;
}
;
    vector = {
  meta = {
    sha256 = "0c1nw2sx14y29afdbwl40sk9vznx71rja5jcg14b8986778kl32d";
    url = "https://hackage.haskell.org";
    ver = "0.13.1.0";
  };
  drv = { mkDerivation, base, base-orphans, deepseq, doctest, HUnit, lib
, primitive, QuickCheck, random, tasty, tasty-bench, tasty-hunit
, tasty-inspection-testing, tasty-quickcheck, template-haskell
, transformers, vector-stream
}:
mkDerivation {
  pname = "vector";
  version = "0.13.1.0";
  src = /nix/store/44g0nh165fmk0mj359hiq2s03ymx3h4h-source;
  libraryHaskellDepends = [ base deepseq primitive vector-stream ];
  testHaskellDepends = [
    base base-orphans doctest HUnit primitive QuickCheck random tasty
    tasty-hunit tasty-inspection-testing tasty-quickcheck
    template-haskell transformers
  ];
  benchmarkHaskellDepends = [ base random tasty tasty-bench ];
  homepage = "https://github.com/haskell/vector";
  description = "Efficient Arrays";
  license = lib.licenses.bsd3;
}
;
}
;
    vector-algorithms = {
  meta = {
    sha256 = "0924b5cif1fm861hl0jwysiv0w6szgpjrn1x90sfli4dvb32ys3c";
    url = "https://hackage.haskell.org";
    ver = "0.9.1.0";
  };
  drv = { mkDerivation, base, bitvec, bytestring, containers, lib
, mwc-random, primitive, QuickCheck, vector
}:
mkDerivation {
  pname = "vector-algorithms";
  version = "0.9.1.0";
  src = /nix/store/p3l2vq0h38l4hg8b0npcikvlqbigpdaa-source;
  libraryHaskellDepends = [
    base bitvec bytestring primitive vector
  ];
  testHaskellDepends = [
    base bytestring containers QuickCheck vector
  ];
  benchmarkHaskellDepends = [ base mwc-random vector ];
  homepage = "https://github.com/erikd/vector-algorithms/";
  description = "Efficient algorithms for vector arrays";
  license = lib.licenses.bsd3;
}
;
}
;
    witch = {
  meta = {
    sha256 = "1w7sbmjc15rn770q3rb7q3j8v7rqw3c7lwaspxlxlx0h48vmdq76";
    url = "https://hackage.haskell.org";
    ver = "1.2.1.1";
  };
  drv = { mkDerivation, base, bytestring, containers, HUnit, lib, tagged
, template-haskell, text, time, transformers
}:
mkDerivation {
  pname = "witch";
  version = "1.2.1.1";
  src = /nix/store/q4b4rv4gh8fbwdmfb6ix05cansxks9sh-source;
  libraryHaskellDepends = [
    base bytestring containers tagged template-haskell text time
  ];
  testHaskellDepends = [
    base bytestring containers HUnit tagged text time transformers
  ];
  description = "Convert values from one type into another";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
;
}
;
    witherable = {
  meta = {
    sha256 = "0xm77dqyfm0zh0xvnh1srwxrkn4sl7m126lqhbzc4q9f6lziwzdx";
    url = "https://hackage.haskell.org";
    ver = "0.5";
  };
  drv = { mkDerivation, base, base-orphans, containers, hashable
, indexed-traversable, indexed-traversable-instances, lib
, QuickCheck, quickcheck-instances, tasty, tasty-quickcheck
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "witherable";
  version = "0.5";
  src = /nix/store/gz5hm6n4glpkkrhb8n86y8xpqa8xakf8-source;
  libraryHaskellDepends = [
    base base-orphans containers hashable indexed-traversable
    indexed-traversable-instances transformers unordered-containers
    vector
  ];
  testHaskellDepends = [
    base containers hashable QuickCheck quickcheck-instances tasty
    tasty-quickcheck transformers unordered-containers vector
  ];
  homepage = "https://github.com/fumieval/witherable";
  description = "filterable traversable";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
;
}
;
    wl-pprint-annotated = {
  meta = {
    sha256 = "0qq7707syvawg5pd4gb6z88klr0fw8c6ncmqpcpqkf3sc34wfljx";
    url = "https://hackage.haskell.org";
    ver = "0.1.0.1";
  };
  drv = { mkDerivation, base, containers, deepseq, lib, tasty, tasty-hunit
, text
}:
mkDerivation {
  pname = "wl-pprint-annotated";
  version = "0.1.0.1";
  src = /nix/store/n04d7y7528w09bdf24fgwsgffzj7m9ab-source;
  libraryHaskellDepends = [ base containers deepseq text ];
  testHaskellDepends = [
    base containers deepseq tasty tasty-hunit text
  ];
  homepage = "https://github.com/minad/wl-pprint-annotated#readme";
  description = "Pretty printer with annotation support";
  license = lib.licenses.bsd3;
}
;
}
;
    yaml = {
  meta = {
    sha256 = "0d9xlsbqf0gwjmqh4r51jnafndan6bf4zj5nklzl6b8x1wnjm7l3";
    url = "https://hackage.haskell.org";
    ver = "0.11.11.2";
  };
  drv = { mkDerivation, aeson, attoparsec, base, base-compat, bytestring
, conduit, containers, directory, filepath, hspec, HUnit, lib
, libyaml, mockery, mtl, raw-strings-qq, resourcet, scientific
, template-haskell, temporary, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "yaml";
  version = "0.11.11.2";
  src = /nix/store/4vy23r3rfbqkxwvm8pg71qaf01lgbaxp-source;
  configureFlags = [ "-fsystem-libyaml" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring conduit containers directory
    filepath libyaml mtl resourcet scientific template-haskell text
    transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson attoparsec base base-compat bytestring conduit containers
    directory filepath hspec HUnit libyaml mockery mtl raw-strings-qq
    resourcet scientific template-haskell temporary text transformers
    unordered-containers vector
  ];
  homepage = "https://github.com/snoyberg/yaml#readme";
  description = "Support for parsing and rendering YAML documents";
  license = lib.licenses.bsd3;
}
;
}
;
  };
}