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
        upper = "2.2";
      };
      effectful-th = {
        lower = "1.0.0.2";
        upper = "1.1";
      };
      h2jvm = {
        lower = null;
        upper = null;
      };
      hedgehog = {
        lower = "1.4";
        upper = "1.8";
      };
      lens = {
        lower = "5.2.3";
        upper = "5.4";
      };
      prettyprinter = {
        lower = "1.7.0";
        upper = "1.8";
      };
      sydtest = {
        lower = null;
        upper = "0.24";
      };
      sydtest-hedgehog = {
        lower = null;
        upper = "0.5";
      };
      text = {
        lower = "1.2.5.0";
        upper = "2.2";
      };
      vector = {
        lower = "0.12.3.1";
        upper = "0.14";
      };
      witch = {
        lower = null;
        upper = "1.4";
      };
    };
  };
  versions = {
    latest = {
      base = "4.21.1.0";
      binary = "0.8.9.3";
      bytestring = "0.12.2.0";
      containers = "0.7";
      effectful = "2.6.1.0";
      effectful-core = "2.6.1.0";
      effectful-plugin = "2.1.0.0";
      effectful-th = "1.0.0.3";
      hedgehog = "1.7";
      lens = "5.3.6";
      prettyprinter = "1.7.1";
      sydtest = "0.23.0.2";
      sydtest-hedgehog = "0.4.0.0";
      text = "2.1.4";
      vector = "0.13.2.0";
      witch = "1.3.1.0";
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
      hedgehog = "1.4";
      lens = "5.2.3";
      prettyprinter = "1.7.0";
      text = "1.2.5.0";
      vector = "0.12.3.1";
    };
  };
  initial = {
    latest = { };
    lower = {
      binary = "0.8.9.1";
      bytestring = "0.12.1.0";
      containers = "0.6.8";
      effectful = "2.6.0.0";
      effectful-core = "2.6.0.0";
      effectful-plugin = "2.0.0.0";
      effectful-th = "1.0.0.2";
      hedgehog = "1.7";
      lens = "5.3";
      prettyprinter = "1.7.0";
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
      aeson = {
        version = "2.3.0.0";
        hash = "0xmdq5pgp66c2wr3ibsh38br7j5zynk9i8i2hvqp820bxh9hi1cw";
        repo = "hackage.haskell.org";
      };
      async = {
        version = "2.2.6";
        hash = "1731pcifiskq6g1b72p34phx85l65ax7mbjw11310b3zwzk0ldyn";
        repo = "hackage.haskell.org";
      };
      attoparsec = {
        version = "0.14.4";
        hash = "0y9dph5axyvr1bfcvmz6qh50bjcp50m2ljra14960anc6g74a3c8";
        repo = "hackage.haskell.org";
      };
      autodocodec = {
        version = "0.5.0.0";
        hash = "0wf3bdgqcn4yi1kbzd655q100vpfsp8idwdsp18r1f9wwgry3laa";
        repo = "hackage.haskell.org";
      };
      autodocodec-nix = {
        version = "0.1.0.0";
        hash = "18prm5vvg8l80vd6arpb0qpg7kv9d8yaw5rnlpcbqq7bmgyl1r1c";
        repo = "hackage.haskell.org";
      };
      autodocodec-schema = {
        version = "0.2.0.1";
        hash = "1gg6cm9ly10f2gpgsvaxb4clkg5wl97xrglpypwfa83cfc65ac9p";
        repo = "hackage.haskell.org";
      };
      autodocodec-yaml = {
        version = "0.4.0.2";
        hash = "18frp5yd314nhz1yf35bwjbn6cs56m6w0hgva8kd4zlrg0jabcd4";
        repo = "hackage.haskell.org";
      };
      blaze-builder = {
        version = "0.4.4.1";
        hash = "17b9bxff50wkmqlbwbnaxwl3bfjax3vk9qchk56ca5xwrvk8nxrd";
        repo = "hackage.haskell.org";
      };
      concurrent-output = {
        version = "1.10.21";
        hash = "1w87rrf337s8wc4z3dkh2mk990003jsk18ry5yawv4465k4yvamw";
        repo = "hackage.haskell.org";
      };
      conduit = {
        version = "1.3.6.1";
        hash = "06hxkbsxa4bgyb8k6apdb94zciczygn33xbm6b5w5y33005xalfx";
        repo = "hackage.haskell.org";
      };
      constraints = {
        version = "0.14.3";
        hash = "19wjba08k68qz7bylc2h5f22vxw4bfk9jpzkdji6zqp87ps82phm";
        repo = "hackage.haskell.org";
      };
      data-fix = {
        version = "0.3.4";
        hash = "0x8r2r8gmdvsclaszg90zn7gla6s8r6salbvgfsp0rscdjzj01ry";
        repo = "hackage.haskell.org";
      };
      effectful = {
        version = "2.6.1.0";
        hash = "1nph9pr2v0q8z236dg7ji6swda75vms6ldsqnxln2vlx38dn7cwj";
        repo = "hackage.haskell.org";
      };
      effectful-plugin = {
        version = "2.1.0.0";
        hash = "1gn4429dy6pbpg0hmj16nb1yp098cpn0fmr7m5v9k557azzixh7a";
        repo = "hackage.haskell.org";
      };
      fast-myers-diff = {
        version = "0.0.1";
        hash = "0csbgddfbf67ghpm1w0416dadl2lyfda4cg72b60whc9l1v6g0k3";
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
        version = "2.11.17";
        hash = "1yizn83bvzml9bgsk052kapnqb8vnpgfqngr1a2485wm7c4gy509";
        repo = "hackage.haskell.org";
      };
      hspec-core = {
        version = "2.11.17";
        hash = "06sj5kdz8k0q1fk0wg0s8v0h5vkxmhcca3adycxs2y0k4iasq9fv";
        repo = "hackage.haskell.org";
      };
      hspec-discover = {
        version = "2.11.17";
        hash = "1yfh8q9809vfczmf3n148r5kl87nd44qwcl1ac07rh6g4wlz7d15";
        repo = "hackage.haskell.org";
      };
      hspec-expectations = {
        version = "0.8.4";
        hash = "0dasnpk5d9s0mazbvhwq6dh6jd67fhbw0s34pinha4mbmb9qcnij";
        repo = "hackage.haskell.org";
      };
      hspec-hedgehog = {
        version = "0.3.0.0";
        hash = "0v6y085nfhb4dwwk3xjmbfnf20fjb9im0q4z68pfgfb9byw9dvly";
        repo = "hackage.haskell.org";
      };
      indexed-profunctors = {
        version = "0.1.1.1";
        hash = "0h92bms2ahpli32g01x9jqwc497xwclkd0234g77kz3896cyzdsk";
        repo = "hackage.haskell.org";
      };
      indexed-traversable-instances = {
        version = "0.1.2";
        hash = "05vpkasz70yjf09hsmbw7nap70sr8p5b7hrsdbmij8k8xqf3qg8r";
        repo = "hackage.haskell.org";
      };
      integer-conversion = {
        version = "0.1.1";
        hash = "0jrch63xc80fq6s14zwi5wcmbrj8zr7anl420sq98aglx3df9yr3";
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
      libyaml = {
        version = "0.1.4";
        hash = "0ng8mjm30bd3jl1jyc5zm6vmz1xjd11dn48vx2kncmihm9g3fvzm";
        repo = "hackage.haskell.org";
      };
      lifted-async = {
        version = "0.11.0";
        hash = "0c0njy8k70swqnp16wyrrkd1bxjsf3pxi34hxka1y1ifp3haccap";
        repo = "hackage.haskell.org";
      };
      mono-traversable = {
        version = "1.0.21.0";
        hash = "1si42kg8b1ic77wbnkzy2x1jlb8gmpxkbnqf8xi2697i00mni3x8";
        repo = "hackage.haskell.org";
      };
      network-uri = {
        version = "2.6.4.2";
        hash = "0zj83viziy80f7nybpmc1hki8wrd8pzps31fxns9vxhc1p7l9chj";
        repo = "hackage.haskell.org";
      };
      opt-env-conf = {
        version = "0.15.0.1";
        hash = "16cvkxn8gb9pw7j1r0ykfi5d66wp1pik4zg1nb5xp14d7xlc58lf";
        repo = "hackage.haskell.org";
      };
      parsec = {
        version = "3.1.18.0";
        hash = "089j939xxi6w6a2ggr40c4s2kdbwkzap2mnhvimmf45hg865h48n";
        repo = "hackage.haskell.org";
      };
      path = {
        version = "0.9.6";
        hash = "16hgrkvd27c9vp5447d1dv3b3fi0fv0jfig10h2j37mzk4850wg8";
        repo = "hackage.haskell.org";
      };
      path-io = {
        version = "1.8.2";
        hash = "063ma7gzqr5c6s8a1yv72jgll3xdajvgclbc8w0ddmqgcrb62x2k";
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
      safe-coloured-text = {
        version = "0.4.0.0";
        hash = "12k8zx42gdrjlaxxxcjmac52gk2kxxsz24i75ngvy1y7b20q8k8z";
        repo = "hackage.haskell.org";
      };
      safe-coloured-text-layout = {
        version = "0.2.0.1";
        hash = "0n2dcd8algzlrlw2vmwjhbpqqm3r2px9g4xjjm9yr03vjqkw3952";
        repo = "hackage.haskell.org";
      };
      safe-coloured-text-terminfo = {
        version = "0.3.0.0";
        hash = "0r569a6f41w07i68js02755kiaakcnsgkn6k791iz9l2q67gs5wj";
        repo = "hackage.haskell.org";
      };
      scientific = {
        version = "0.3.8.1";
        hash = "0imbwigr1m378bk51gc2d8cbrj5r8sdv3bgvn0386lc07sayp3ng";
        repo = "hackage.haskell.org";
      };
      semialign = {
        version = "1.4";
        hash = "1hanj5gkmk9sbzbx6zbk4xin9s4vgk1zidk100kvg20xsgfpkwi0";
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
      svg-builder = {
        version = "0.1.1";
        hash = "1r2d06kafz2ahhbj1sip9liqv04zcmp1rciwx062m6880zm3pklm";
        repo = "hackage.haskell.org";
      };
      sydtest = {
        version = "0.23.0.2";
        hash = "0l0hbi44cjwic7nczs9nfwywlajg6acr3ja7nr4vz9pa7kw4r38d";
        repo = "hackage.haskell.org";
      };
      sydtest-hedgehog = {
        version = "0.4.0.0";
        hash = "06an6p0mhdxm4rhqnhd92f2k248b70fs67xxg74v0v3g7l27iy1s";
        repo = "hackage.haskell.org";
      };
      text = {
        version = "2.1.4";
        hash = "1rdjjanxj5pr5y73h7bss3lh0x8w9yml9kzir4amlh1sxqlf17rd";
        repo = "hackage.haskell.org";
      };
      text-iso8601 = {
        version = "0.2.0.0";
        hash = "0qh0lgfd0rav0wa93chi983jyhdqzalrj5ywrvv65fnig111nv0h";
        repo = "hackage.haskell.org";
      };
      text-short = {
        version = "0.1.6.1";
        hash = "1yzyzklry9cdc12283b0zf0kpa8nb7gixmdaf3l8x7388zpxhhay";
        repo = "hackage.haskell.org";
      };
      these = {
        version = "1.2.1";
        hash = "0jqchlmycfcvkff48shhkswansnzrw57q8945m483mrd59zpg27k";
        repo = "hackage.haskell.org";
      };
      time-compat = {
        version = "1.9.9";
        hash = "02yq6qc9fbawpxkypaf4nm9vidfv5vvgidxyj4r3dxa4lb29jd2p";
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
      uuid-types = {
        version = "1.0.6.1";
        hash = "1jrid43smmfcchrfwpzkxil16a4c5016y4b49yjka0sildj1lprg";
        repo = "hackage.haskell.org";
      };
      validity-aeson = {
        version = "0.2.0.5";
        hash = "0wirg6wjzpc37073dia3c82i2svihn2bbh31kxwqxjl6zqw5a1ia";
        repo = "hackage.haskell.org";
      };
      validity-scientific = {
        version = "0.2.0.3";
        hash = "1mf2cj90a9y2vbhmsdqfni8by1a3ngzjr4yg3fafq13vhnsylg3m";
        repo = "hackage.haskell.org";
      };
      validity-text = {
        version = "0.3.1.3";
        hash = "0mmlyzfz95j0s81qj7l42vqdl1gdnaa98z90cks8jp4kjxzd6ymp";
        repo = "hackage.haskell.org";
      };
      validity-unordered-containers = {
        version = "0.2.0.3";
        hash = "01zbbdq3c042w7qwh246nyvd4wmr1f816l1mzfvh1ji35892w7qh";
        repo = "hackage.haskell.org";
      };
      validity-vector = {
        version = "0.2.0.3";
        hash = "09bf0k76v7iyj7q1sn8kzn1xgdf1f8zfrhnss2mddm42yb8g114p";
        repo = "hackage.haskell.org";
      };
      witch = {
        version = "1.3.1.0";
        hash = "1n848h6dsyflpn7hn72pjnz3j06fg7i6gay1w6namjy597miyg1l";
        repo = "hackage.haskell.org";
      };
      witherable = {
        version = "0.5";
        hash = "0xm77dqyfm0zh0xvnh1srwxrkn4sl7m126lqhbzc4q9f6lziwzdx";
        repo = "hackage.haskell.org";
      };
      wl-pprint-annotated = {
        version = "0.1.0.1";
        hash = "0qq7707syvawg5pd4gb6z88klr0fw8c6ncmqpcpqkf3sc34wfljx";
        repo = "hackage.haskell.org";
      };
      yaml = {
        version = "0.11.11.2";
        hash = "0d9xlsbqf0gwjmqh4r51jnafndan6bf4zj5nklzl6b8x1wnjm7l3";
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
    latest = { };
    lower = { };
  };
  packages = { };
  resolving = false;
}
