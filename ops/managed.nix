{
  bounds = {
    h2jvm = {
      base = {
        lower = "4.19.2.0";
        upper = "4.22";
      };
      binary = {
        lower = "0.8.9.1";
        upper = "0.9";
      };
      bytestring = {
        lower = "0.12.1.0";
        upper = "0.13";
      };
      containers = {
        lower = "0.6.8";
        upper = "0.8";
      };
      effectful = {
        lower = "2.2.2.0";
        upper = "2.7";
      };
      effectful-core = {
        lower = "2.2.2.0";
        upper = "2.7";
      };
      effectful-plugin = {
        lower = "1.1.0.2";
        upper = "2.1";
      };
      effectful-th = {
        lower = "1.0.0.2";
        upper = "1.1";
      };
      generic-lens = {
        lower = "2.2.1.0";
        upper = "2.4";
      };
      h2jvm = {
        lower = null;
        upper = null;
      };
      hedgehog = {
        lower = "1.4";
        upper = "1.8";
      };
      hspec = {
        lower = "2.11.0";
        upper = "2.12";
      };
      hspec-hedgehog = {
        lower = "0.0.1.0";
        upper = "0.4";
      };
      lens = {
        lower = "5.2.3";
        upper = "5.4";
      };
      prettyprinter = {
        lower = "1.7.0";
        upper = "1.8";
      };
      split = {
        lower = "0.1.2.3";
        upper = "0.3";
      };
      text = {
        lower = "1.2.5.0";
        upper = "2.2";
      };
      vector = {
        lower = "0.12.3.1";
        upper = "0.14";
      };
    };
  };
  versions = {
    latest = {
      base = "4.21.0.0";
      binary = "0.8.9.3";
      bytestring = "0.12.2.0";
      containers = "0.7";
      effectful = "2.6.1.0";
      effectful-core = "2.6.1.0";
      effectful-plugin = "2.0.0.1";
      effectful-th = "1.0.0.3";
      generic-lens = "2.3.0.0";
      hedgehog = "1.7";
      hspec = "2.11.16";
      hspec-hedgehog = "0.3.0.0";
      lens = "5.3.6";
      prettyprinter = "1.7.1";
      split = "0.2.5";
      text = "2.1.3";
      vector = "0.13.2.0";
    };
    lower = {
      base = "4.19.2.0";
      binary = "0.8.9.1";
      bytestring = "0.12.1.0";
      containers = "0.6.8";
      effectful = "2.2.2.0";
      effectful-core = "2.2.2.0";
      effectful-plugin = "1.1.0.2";
      effectful-th = "1.0.0.2";
      generic-lens = "2.2.1.0";
      hedgehog = "1.4";
      hspec = "2.11.0";
      hspec-hedgehog = "0.0.1.0";
      lens = "5.2.3";
      prettyprinter = "1.7.0";
      split = "0.1.2.3";
      text = "1.2.5.0";
      vector = "0.12.3.1";
    };
  };
  initial = {
    latest = {};
    lower = {
      binary = "0.8.9.1";
      bytestring = "0.12.1.0";
      containers = "0.6.8";
      effectful = "2.6.0.0";
      effectful-core = "2.6.0.0";
      effectful-plugin = "2.0.0.0";
      effectful-th = "1.0.0.2";
      generic-lens = "2.3.0.0";
      hedgehog = "1.7";
      hspec = "2.11.0";
      hspec-hedgehog = "0.3.0.0";
      lens = "5.3";
      prettyprinter = "1.7.0";
      split = "0.2.3.5";
      text = "2.1.1";
      vector = "0.12.3.1";
    };
  };
  overrides = {
    latest = {
      adjunctions = {
        version = "4.4.4";
        hash = "0bqp5wmabksajw50bcfhvab3gda9hsp04y5abkp6zfnhmq2v1r2y";
        repo = "hackage.haskell.org";
      };
      async = {
        version = "2.2.6";
        hash = "1731pcifiskq6g1b72p34phx85l65ax7mbjw11310b3zwzk0ldyn";
        repo = "hackage.haskell.org";
      };
      concurrent-output = {
        version = "1.10.21";
        hash = "1w87rrf337s8wc4z3dkh2mk990003jsk18ry5yawv4465k4yvamw";
        repo = "hackage.haskell.org";
      };
      constraints = {
        version = "0.14.3";
        hash = "19wjba08k68qz7bylc2h5f22vxw4bfk9jpzkdji6zqp87ps82phm";
        repo = "hackage.haskell.org";
      };
      effectful = {
        version = "2.6.1.0";
        hash = "1nph9pr2v0q8z236dg7ji6swda75vms6ldsqnxln2vlx38dn7cwj";
        repo = "hackage.haskell.org";
      };
      free = {
        version = "5.2";
        hash = "0b646kh0jwyswi548z1maqjircac4c80zfm0fz06jr0yd0ydrjq1";
        repo = "hackage.haskell.org";
      };
      generic-lens = {
        version = "2.3.0.0";
        hash = "06q0ghaj90hqp0chb3z5qzr3cx8ypanjk24d4wnb1b7b8s13rhsp";
        repo = "hackage.haskell.org";
      };
      generic-lens-core = {
        version = "2.3.0.0";
        hash = "05im3y27lhjjy6hi0i85rlqsan510fmp63lqfwg18cnlzn0yvf81";
        repo = "hackage.haskell.org";
      };
      happy = {
        version = "2.2";
        hash = "11xfm7y0dxb676635xqcfgqr0syq9j3hy1157f3kxpb3ljsyg85a";
        repo = "hackage.haskell.org";
      };
      happy-lib = {
        version = "2.2";
        hash = "1j83gcfi1w11p9yb87b543lmkbf3xajyfbid7y2mv0s75jsvqgym";
        repo = "hackage.haskell.org";
      };
      hashable = {
        version = "1.5.1.0";
        hash = "02mk0fxkqrx11qffs7jl231bfflz10vyx5s5xqn8y7ayyndmb6db";
        repo = "hackage.haskell.org";
      };
      hedgehog = {
        version = "1.7";
        hash = "04cjnz4i1qs3v9bza8a3ry1czapwqgxazhywkjzq2rg1544gjmby";
        repo = "hackage.haskell.org";
      };
      hspec = {
        version = "2.11.16";
        hash = "05yij7v8ai1rqfq531h41p6qan9mq8vxdjilxdacbszi2v12i5jw";
        repo = "hackage.haskell.org";
      };
      hspec-core = {
        version = "2.11.16";
        hash = "11m5b0gvld3k3v1dpx95dib1y3g0znyrfhf6jq9jgwrd529hni0p";
        repo = "hackage.haskell.org";
      };
      hspec-discover = {
        version = "2.11.16";
        hash = "1zw59zjbf9ibw7nx7s2bahdb7i96j3szl73y1bf95yp2h5x7psbn";
        repo = "hackage.haskell.org";
      };
      hspec-hedgehog = {
        version = "0.3.0.0";
        hash = "0v6y085nfhb4dwwk3xjmbfnf20fjb9im0q4z68pfgfb9byw9dvly";
        repo = "hackage.haskell.org";
      };
      indexed-traversable-instances = {
        version = "0.1.2";
        hash = "05vpkasz70yjf09hsmbw7nap70sr8p5b7hrsdbmij8k8xqf3qg8r";
        repo = "hackage.haskell.org";
      };
      invariant = {
        version = "0.6.5";
        hash = "1arihzidi3jkn26l01mgql4dk3iqm5rl6ns4swr79vqi8i3k4qkx";
        repo = "hackage.haskell.org";
      };
      kan-extensions = {
        version = "5.2.8";
        hash = "002j5356ls1gcik2rrmjg10vk1p6g6n0hjf7h1x96zab1k4z21bc";
        repo = "hackage.haskell.org";
      };
      lens = {
        version = "5.3.6";
        hash = "17g77mqcyy83lxrhb9lnjnp6m38mgphyzkaajy8kf00c0a41lyya";
        repo = "hackage.haskell.org";
      };
      lifted-async = {
        version = "0.11.0";
        hash = "0c0njy8k70swqnp16wyrrkd1bxjsf3pxi34hxka1y1ifp3haccap";
        repo = "hackage.haskell.org";
      };
      pretty-show = {
        version = "1.10";
        hash = "1q3pkp0ly221yf2r3skr6v0664bb0a6z7x82hvy6yl02ds2g9b1n";
        repo = "hackage.haskell.org";
      };
      prettyprinter = {
        version = "1.7.1";
        hash = "0ddf0wb06sqipklh00ah3wazy37g8hnnm99n8g96xmwbhakmpaz2";
        repo = "hackage.haskell.org";
      };
      semigroupoids = {
        version = "6.0.2";
        hash = "0nc2c573inxnp4nz3pbahb66ca9750zdgashwnak7kxyrq7d763l";
        repo = "hackage.haskell.org";
      };
      strict = {
        version = "0.5.1";
        hash = "06y3ab0nsdbrkrxzc7hgy6cwxl72wcgqn52bs1vvi5lkp64v559y";
        repo = "hackage.haskell.org";
      };
      text = {
        version = "2.1.3";
        hash = "1wwbsjp63s0g0cb57rrgracvkjnsymcjsxwgfm6waw3dgczi3qpc";
        repo = "hackage.haskell.org";
      };
      these = {
        version = "1.2.1";
        hash = "0jqchlmycfcvkff48shhkswansnzrw57q8945m483mrd59zpg27k";
        repo = "hackage.haskell.org";
      };
      unliftio = {
        version = "0.2.25.1";
        hash = "0cp92d9f2hzya636y7w8m0gw7ik6ri2clzpdnz5klh917nnbd7ii";
        repo = "hackage.haskell.org";
      };
      unordered-containers = {
        version = "0.2.21";
        hash = "0na84q5vxxww3pmz72ihpx4j7dhk71z28r55i7j0pq7mj27jasb0";
        repo = "hackage.haskell.org";
      };
      wl-pprint-annotated = {
        version = "0.1.0.1";
        hash = "0qq7707syvawg5pd4gb6z88klr0fw8c6ncmqpcpqkf3sc34wfljx";
        repo = "hackage.haskell.org";
      };
    };
    lower = {
      QuickCheck = {
        version = "2.14.3";
        hash = "18451rdmih1jkrsrckdcix71zqihc4h2caic7qzizxjg4hqpapji";
        repo = "hackage.haskell.org";
      };
      adjunctions = {
        version = "4.4.3";
        hash = "1k940bpa8qiqrj6hh8jh2vshdfdisi827i3l6m2hwajc4yhjlrgg";
        repo = "hackage.haskell.org";
      };
      async = {
        version = "2.2.5";
        hash = "1y1cgzi8fy8qhyd69q7avhx46sz5h5ljggh81azr67rqrdhdyf4a";
        repo = "hackage.haskell.org";
      };
      concurrent-output = {
        version = "1.10.21";
        hash = "1w87rrf337s8wc4z3dkh2mk990003jsk18ry5yawv4465k4yvamw";
        repo = "hackage.haskell.org";
      };
      constraints = {
        version = "0.14.2";
        hash = "1nnlw9q7z2yvdaadyanvxqqwzb67kmk0n59xnc2pz0fhvqmq3yh8";
        repo = "hackage.haskell.org";
      };
      effectful = {
        version = "2.2.2.0";
        hash = "0hwx2mna18ydmfqcirakdx6qfgjhnv7hnr510a2ghf6ccs0gwgzn";
        repo = "hackage.haskell.org";
      };
      effectful-core = {
        version = "2.2.2.0";
        hash = "0ndifywpjy7zv2ghgz7v78il890n6gzi2fgkc0avr4alh2w9apn8";
        repo = "hackage.haskell.org";
      };
      effectful-plugin = {
        version = "1.1.0.2";
        hash = "0g5rddi7gxa40m3v0l9z7i19lv8a6nvq6dc6xfz5vf6513g93n96";
        repo = "hackage.haskell.org";
      };
      effectful-th = {
        version = "1.0.0.2";
        hash = "162virkpn2fql7l8q9xw74xy4bbg4xb3wyr2wsmys6vxz9mlzh4n";
        repo = "hackage.haskell.org";
      };
      free = {
        version = "5.2";
        hash = "0b646kh0jwyswi548z1maqjircac4c80zfm0fz06jr0yd0ydrjq1";
        repo = "hackage.haskell.org";
      };
      generic-lens = {
        version = "2.2.1.0";
        hash = "0cd3w5hpf0yqi1vrkxzm4qlc2n797fdmhyhcvkrz4ym19v2vylyb";
        repo = "hackage.haskell.org";
      };
      generic-lens-core = {
        version = "2.2.1.0";
        hash = "0innx8ndljssasw9f1fnhwhgjv3smn57kir895d7fbj4r0k8w11s";
        repo = "hackage.haskell.org";
      };
      happy = {
        version = "2.1.7";
        hash = "0qigxkcnpzj1fg35h07jlzsj360bwhik3nxycm1ni46vn5ybijs4";
        repo = "hackage.haskell.org";
      };
      happy-lib = {
        version = "2.1.7";
        hash = "0ylgm8kxgl1navcd0k227163h8i81xs5cxryg79mjzmjzfhix85k";
        repo = "hackage.haskell.org";
      };
      hashable = {
        version = "1.4.7.0";
        hash = "1zfkla3kjd7b4w5bd93vv71f8gj5849vi924j3kl68cj1njk8i6a";
        repo = "hackage.haskell.org";
      };
      hedgehog = {
        version = "1.4";
        hash = "1qxxhs720im0wpa5lsca0l8qsfmhbyphd1aq01nv96v29lgv795b";
        repo = "hackage.haskell.org";
      };
      hspec = {
        version = "2.11.0";
        hash = "1wyjvf0hsj3f8wc665x13njv7kvl35880q7rbj1b2fpysqs5xzwp";
        repo = "hackage.haskell.org";
      };
      hspec-core = {
        version = "2.11.0";
        hash = "1n4xdrcrq0kx4bwwhwza4flxa9dv14b30264i1czp21s4y19wln4";
        repo = "hackage.haskell.org";
      };
      hspec-discover = {
        version = "2.11.0";
        hash = "1n4qxmp1vhzhh37hqnffbg33s39qkjzf5xqqdlylq04rlxabyx1w";
        repo = "hackage.haskell.org";
      };
      hspec-expectations = {
        version = "0.8.2";
        hash = "0dnd64gi37xdav3yg1r26qqwa41g56pinb8ib11a798ahzk2375v";
        repo = "hackage.haskell.org";
      };
      hspec-hedgehog = {
        version = "0.0.1.0";
        hash = "0f19l51fx4ys833lr3xzz81jzfy1yg0lfkw8156flrmn7bpmwlna";
        repo = "hackage.haskell.org";
      };
      indexed-traversable-instances = {
        version = "0.1.1.2";
        hash = "1mmkklfpagv855p12dqq0r6xwg0v6dc1gj1n3nvzzy4b909ajgd0";
        repo = "hackage.haskell.org";
      };
      invariant = {
        version = "0.6.4";
        hash = "0rf1jxskxvkinjxywg91yx0nxk4rfaafw82jwl6akb3vxx2bsc61";
        repo = "hackage.haskell.org";
      };
      kan-extensions = {
        version = "5.2.7";
        hash = "0a41sk3118wv9nfsr1zd75m94m6l8p655kq6dm6hq9sdjf233rag";
        repo = "hackage.haskell.org";
      };
      lens = {
        version = "5.2.3";
        hash = "0wh2iasdvjk3b2jkrjmypx56zpz3ks2bdwczwkqblws1j7g8bajh";
        repo = "hackage.haskell.org";
      };
      lifted-async = {
        version = "0.10.2.7";
        hash = "0cgzs8sfr3l7ah5nnscpp50v5mmvc4hqf02zdi4h344dbbha10fy";
        repo = "hackage.haskell.org";
      };
      pretty-show = {
        version = "1.10";
        hash = "1q3pkp0ly221yf2r3skr6v0664bb0a6z7x82hvy6yl02ds2g9b1n";
        repo = "hackage.haskell.org";
      };
      prettyprinter = {
        version = "1.7.0";
        hash = "17byy08brwcsl5rqdhibq3pcpgx085shizb2ap6s4xy3izdia3cc";
        repo = "hackage.haskell.org";
      };
      quickcheck-io = {
        version = "0.2.0";
        hash = "1flw5h455z6vr0ghv1i4z1rc033jp1hcv4ci1jnqryd6pyh95ny3";
        repo = "hackage.haskell.org";
      };
      semigroupoids = {
        version = "6.0.1";
        hash = "10qd2y5f5m7jzrha1wfbwwybhhghdwkdmk9ajybdz8h88cz9ig2g";
        repo = "hackage.haskell.org";
      };
      split = {
        version = "0.1.2.3";
        hash = "1n962sg4i1yvvli4w3m2hg949kbf4mycmhviqgw68cgdv9nxi9kf";
        repo = "hackage.haskell.org";
      };
      strict = {
        version = "0.5.1";
        hash = "06y3ab0nsdbrkrxzc7hgy6cwxl72wcgqn52bs1vvi5lkp64v559y";
        repo = "hackage.haskell.org";
      };
      text = {
        version = "1.2.5.0";
        hash = "1v6wjya4i736vn6nv8vhh6nhfwlcvjlj0dz882445v06gyicrlql";
        repo = "hackage.haskell.org";
      };
      these = {
        version = "1.2.1";
        hash = "0jqchlmycfcvkff48shhkswansnzrw57q8945m483mrd59zpg27k";
        repo = "hackage.haskell.org";
      };
      unliftio = {
        version = "0.2.25.1";
        hash = "0cp92d9f2hzya636y7w8m0gw7ik6ri2clzpdnz5klh917nnbd7ii";
        repo = "hackage.haskell.org";
      };
      unordered-containers = {
        version = "0.2.21";
        hash = "0na84q5vxxww3pmz72ihpx4j7dhk71z28r55i7j0pq7mj27jasb0";
        repo = "hackage.haskell.org";
      };
      vector = {
        version = "0.12.3.1";
        hash = "17jadyf0qkk1g0d1qnp8hi2aaf0ydc79ncrhswnv1dw9vrpp74w4";
        repo = "hackage.haskell.org";
      };
      wl-pprint-annotated = {
        version = "0.1.0.1";
        hash = "0qq7707syvawg5pd4gb6z88klr0fw8c6ncmqpcpqkf3sc34wfljx";
        repo = "hackage.haskell.org";
      };
    };
  };
  solver = {
    latest = {};
    lower = {};
  };
  resolving = false;
}
