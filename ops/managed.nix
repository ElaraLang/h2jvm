{
  bounds = {
    h2jvm = {
      base = {
        lower = null;
        upper = "4.22";
      };
      binary = {
        lower = null;
        upper = "0.9";
      };
      bytestring = {
        lower = null;
        upper = "0.13";
      };
      containers = {
        lower = null;
        upper = "0.8";
      };
      effectful = {
        lower = null;
        upper = "2.7";
      };
      effectful-core = {
        lower = null;
        upper = "2.7";
      };
      effectful-plugin = {
        lower = null;
        upper = "2.1";
      };
      effectful-th = {
        lower = null;
        upper = "1.1";
      };
      h2jvm = {
        lower = null;
        upper = null;
      };
      hedgehog = {
        lower = null;
        upper = "1.8";
      };
      lens = {
        lower = null;
        upper = "5.4";
      };
      prettyprinter = {
        lower = null;
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
        lower = null;
        upper = "2.2";
      };
      vector = {
        lower = null;
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
    lower = { };
  };
  initial = {
    latest = { };
    lower = { };
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
  };
  solver = {
    latest = { };
  };
  packages = { };
  resolving = false;
}
