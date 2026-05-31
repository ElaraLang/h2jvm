{
  bounds = {
    h2jvm = {
      base = {
        lower = "4.20.2.0";
        upper = "4.22";
      };
      binary = {
        lower = "0.8.9.3";
        upper = "0.9";
      };
      bytestring = {
        lower = "0.12.2.0";
        upper = "0.13";
      };
      containers = {
        lower = "0.7";
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
        lower = "1.1.0.3";
        upper = "2.1";
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
        lower = "5.3";
        upper = "5.4";
      };
      prettyprinter = {
        lower = "1.7.0";
        upper = "1.8";
      };
      sydtest = {
        lower = "0.16.0.0";
        upper = "0.24";
      };
      sydtest-hedgehog = {
        lower = "0.4.0.0";
        upper = "0.5";
      };
      text = {
        lower = "2.0.2";
        upper = "2.2";
      };
      vector = {
        lower = "0.13.1.0";
        upper = "0.14";
      };
      witch = {
        lower = "1.2.1.1";
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
      effectful-plugin = "2.0.0.1";
      effectful-th = "1.0.0.3";
      hedgehog = "1.7";
      lens = "5.3.6";
      prettyprinter = "1.7.1";
      sydtest = "0.23.0.2";
      sydtest-hedgehog = "0.4.0.0";
      text = "2.1.3";
      vector = "0.13.2.0";
      witch = "1.3.1.0";
    };
    lower = {
      base = "4.20.2.0";
      binary = "0.8.9.3";
      bytestring = "0.12.2.0";
      containers = "0.7";
      effectful = "2.2.2.0";
      effectful-core = "2.2.2.0";
      effectful-plugin = "1.1.0.3";
      effectful-th = "1.0.0.2";
      hedgehog = "1.4";
      lens = "5.3";
      prettyprinter = "1.7.0";
      sydtest = "0.16.0.0";
      sydtest-hedgehog = "0.4.0.0";
      text = "2.0.2";
      vector = "0.13.1.0";
      witch = "1.2.1.1";
    };
  };
  initial = {
    latest = {};
    lower = {
      binary = "0.8.9.3";
      bytestring = "0.12.2.0";
      containers = "0.7";
      effectful = "2.6.0.0";
      effectful-core = "2.6.0.0";
      effectful-plugin = "2.0.0.0";
      effectful-th = "1.0.0.2";
      hedgehog = "1.7";
      lens = "5.3";
      prettyprinter = "1.7.0";
      sydtest = "0.23.0.0";
      sydtest-hedgehog = "0.4.0.0";
      text = "2.1.1";
      vector = "0.13.1.0";
      witch = "1.3.0.0";
    };
  };
  overrides = {
    latest = {
      hedgehog = {
        version = "1.7";
        hash = "04cjnz4i1qs3v9bza8a3ry1czapwqgxazhywkjzq2rg1544gjmby";
        repo = "hackage.haskell.org";
      };
      opt-env-conf = {
        version = "0.15.0.1";
        hash = "16cvkxn8gb9pw7j1r0ykfi5d66wp1pik4zg1nb5xp14d7xlc58lf";
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
    };
    lower = {
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
      bitvec = {
        version = "1.1.6.0";
        hash = "15rc25nlf8s6kxw7wfplma6znpc6sh2vmginyb5qdyhjidyzglpg";
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
        version = "0.14.4";
        hash = "00cjd15kn30qgq541s0g3sd2lnvrdswx3bkafk0bmrg9b0kdb6hg";
        repo = "hackage.haskell.org";
      };
      data-fix = {
        version = "0.3.4";
        hash = "0x8r2r8gmdvsclaszg90zn7gla6s8r6salbvgfsp0rscdjzj01ry";
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
        version = "1.1.0.3";
        hash = "1gv38zzay4yplcfsm1dibm3vkiymdpic358kz3d7d8a6ygfr00zv";
        repo = "hackage.haskell.org";
      };
      effectful-th = {
        version = "1.0.0.2";
        hash = "162virkpn2fql7l8q9xw74xy4bbg4xb3wyr2wsmys6vxz9mlzh4n";
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
        version = "1.4.7.0";
        hash = "1zfkla3kjd7b4w5bd93vv71f8gj5849vi924j3kl68cj1njk8i6a";
        repo = "hackage.haskell.org";
      };
      hedgehog = {
        version = "1.4";
        hash = "1qxxhs720im0wpa5lsca0l8qsfmhbyphd1aq01nv96v29lgv795b";
        repo = "hackage.haskell.org";
      };
      indexed-traversable-instances = {
        version = "0.1.2.1";
        hash = "1issj9yfpxnshm6k7xq3wmmgrhn87cb0jalp0d1ls3zqx0qjrr03";
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
        version = "5.3";
        hash = "0y6f4nflzbyk93xzcc0w842ya7b4waa1llj07cqaq5cy5bwsr4ia";
        repo = "hackage.haskell.org";
      };
      libyaml = {
        version = "0.1.4";
        hash = "0ng8mjm30bd3jl1jyc5zm6vmz1xjd11dn48vx2kncmihm9g3fvzm";
        repo = "hackage.haskell.org";
      };
      lifted-async = {
        version = "0.10.2.7";
        hash = "0cgzs8sfr3l7ah5nnscpp50v5mmvc4hqf02zdi4h344dbbha10fy";
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
        version = "1.7.0";
        hash = "17byy08brwcsl5rqdhibq3pcpgx085shizb2ap6s4xy3izdia3cc";
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
        version = "0.16.0.0";
        hash = "1g2w1sn1bi22s7vbb9ss9kgi66jplsc122mvsnjsn492g0xjxwkb";
        repo = "hackage.haskell.org";
      };
      sydtest-hedgehog = {
        version = "0.4.0.0";
        hash = "06an6p0mhdxm4rhqnhd92f2k248b70fs67xxg74v0v3g7l27iy1s";
        repo = "hackage.haskell.org";
      };
      text = {
        version = "2.0.2";
        hash = "1gi9f9karjfl577bmkgd5ldygq68f54nfw8hwpqlsf0b5n4f14s8";
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
      vector = {
        version = "0.13.1.0";
        hash = "0c1nw2sx14y29afdbwl40sk9vznx71rja5jcg14b8986778kl32d";
        repo = "hackage.haskell.org";
      };
      vector-algorithms = {
        version = "0.9.1.0";
        hash = "0924b5cif1fm861hl0jwysiv0w6szgpjrn1x90sfli4dvb32ys3c";
        repo = "hackage.haskell.org";
      };
      witch = {
        version = "1.2.1.1";
        hash = "1w7sbmjc15rn770q3rb7q3j8v7rqw3c7lwaspxlxlx0h48vmdq76";
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
  };
  solver = {
    latest = {};
    lower = {};
  };
  packages = {};
  resolving = false;
}
