{
dev = {
};
ghc910 = {
};
ghc912 = {
};
ghc98 = {
};
hix-build-tools = {
};
hls = {
};
latest = {
  generic-lens = {
  meta = {
    sha256 = "06q0ghaj90hqp0chb3z5qzr3cx8ypanjk24d4wnb1b7b8s13rhsp";
    url = "https://hackage.haskell.org";
    ver = "2.3.0.0";
  };
  drv = { mkDerivation, base, doctest, generic-lens-core, HUnit
, inspection-testing, lens, lib, mtl, profunctors
}:
mkDerivation {
  pname = "generic-lens";
  version = "2.3.0.0";
  src = /nix/store/fi8256z790q44j9l9w91qpip94gf5494-source;
  libraryHaskellDepends = [ base generic-lens-core profunctors ];
  testHaskellDepends = [
    base doctest HUnit inspection-testing lens mtl
  ];
  homepage = "https://github.com/kcsongor/generic-lens";
  description = "Generically derive traversals, lenses and prisms";
  license = lib.licenses.bsd3;
}
;
}
;
  generic-lens-core = {
  meta = {
    sha256 = "05im3y27lhjjy6hi0i85rlqsan510fmp63lqfwg18cnlzn0yvf81";
    url = "https://hackage.haskell.org";
    ver = "2.3.0.0";
  };
  drv = { mkDerivation, base, indexed-profunctors, lib, text }:
mkDerivation {
  pname = "generic-lens-core";
  version = "2.3.0.0";
  src = /nix/store/d0648wfd6zvrini3699ybcf9vzfm47z5-source;
  libraryHaskellDepends = [ base indexed-profunctors text ];
  homepage = "https://github.com/kcsongor/generic-lens";
  description = "Generically derive traversals, lenses and prisms";
  license = lib.licenses.bsd3;
}
;
}
;
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
  hspec = {
  meta = {
    sha256 = "05yij7v8ai1rqfq531h41p6qan9mq8vxdjilxdacbszi2v12i5jw";
    url = "https://hackage.haskell.org";
    ver = "2.11.16";
  };
  drv = { mkDerivation, base, hspec-core, hspec-discover
, hspec-expectations, lib, QuickCheck
}:
mkDerivation {
  pname = "hspec";
  version = "2.11.16";
  src = /nix/store/xmy0kq1alvzgkcfz55vyybz87lwzzqa5-source;
  libraryHaskellDepends = [
    base hspec-core hspec-discover hspec-expectations QuickCheck
  ];
  homepage = "https://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = lib.licenses.mit;
}
;
}
;
  hspec-core = {
  meta = {
    sha256 = "11m5b0gvld3k3v1dpx95dib1y3g0znyrfhf6jq9jgwrd529hni0p";
    url = "https://hackage.haskell.org";
    ver = "2.11.16";
  };
  drv = { mkDerivation, ansi-terminal, array, base, base-orphans
, call-stack, containers, deepseq, directory, filepath
, haskell-lexer, hspec-expectations, hspec-meta, HUnit, lib
, process, QuickCheck, quickcheck-io, random, silently, stm
, temporary, time, transformers
}:
mkDerivation {
  pname = "hspec-core";
  version = "2.11.16";
  src = /nix/store/m04vbifpilvjd4hl5926dciab84kmvk6-source;
  libraryHaskellDepends = [
    ansi-terminal array base call-stack containers deepseq directory
    filepath haskell-lexer hspec-expectations HUnit process QuickCheck
    quickcheck-io random stm time transformers
  ];
  testHaskellDepends = [
    ansi-terminal array base base-orphans call-stack containers deepseq
    directory filepath haskell-lexer hspec-expectations hspec-meta
    HUnit process QuickCheck quickcheck-io random silently stm
    temporary time transformers
  ];
  testToolDepends = [ hspec-meta ];
  testFlags = [
    "--skip"
    "'Test.Hspec.Core.Runner.hspecResult runs specs in parallel'"
  ];
  homepage = "https://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = lib.licenses.mit;
}
;
}
;
  hspec-discover = {
  meta = {
    sha256 = "1zw59zjbf9ibw7nx7s2bahdb7i96j3szl73y1bf95yp2h5x7psbn";
    url = "https://hackage.haskell.org";
    ver = "2.11.16";
  };
  drv = { mkDerivation, base, directory, filepath, hspec-meta, lib, mockery
, QuickCheck
}:
mkDerivation {
  pname = "hspec-discover";
  version = "2.11.16";
  src = /nix/store/4ayvvqhhpzv8i238zbgv2rx6gmc8p9g9-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory filepath ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base directory filepath hspec-meta mockery QuickCheck
  ];
  testToolDepends = [ hspec-meta ];
  homepage = "https://hspec.github.io/";
  description = "Automatically discover and run Hspec tests";
  license = lib.licenses.mit;
  mainProgram = "hspec-discover";
}
;
}
;
  hspec-hedgehog = {
  meta = {
    sha256 = "0v6y085nfhb4dwwk3xjmbfnf20fjb9im0q4z68pfgfb9byw9dvly";
    url = "https://hackage.haskell.org";
    ver = "0.3.0.0";
  };
  drv = { mkDerivation, base, hedgehog, hspec, hspec-core, hspec-discover
, HUnit, lib, QuickCheck, splitmix
}:
mkDerivation {
  pname = "hspec-hedgehog";
  version = "0.3.0.0";
  src = /nix/store/y02mbgg23lk844jjj4x4scwpizv7lkdw-source;
  libraryHaskellDepends = [
    base hedgehog hspec hspec-core QuickCheck splitmix
  ];
  testHaskellDepends = [
    base hedgehog hspec hspec-core HUnit QuickCheck
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/hspec/hspec-hedgehog#readme";
  description = "Integrate Hedgehog and Hspec!";
  license = lib.licenses.bsd3;
}
;
}
;
};
min = {
};
profiled = {
};
}