{ fetchdarcs, stdenv, perl, ghc }:

let pappysrc = fetchdarcs {
      url = "https://mbays.sdf.org/pappy";
      context = ./pappy.context;
      sha256 = "16d5j7bis2i8fr7hj3458r9grh91mm6k6b27z4srhi3ppmbx21sh";
    };

    version = "0.1.0.3";

 in stdenv.mkDerivation {
      name = "pappy";
      inherit version;
      src = "${pappysrc}/pappy";
      buildInputs = [ perl ghc ];
      installPhase = ''
        mkdir -p $out/bin
        cp -r /build/pappy/pappy $out/bin/
      '';
      meta = {
        inherit version;
        homepage = "http://pdos.csail.mit.edu/~baford/packrat/thesis/";
        description = "Packrat parsing; linear-time parsers for grammars in TDPL";
        license = stdenv.lib.licenses.bsd3;
        maintainer = (import ./nix/maintainers.nix).lboklin;
      };
    }
