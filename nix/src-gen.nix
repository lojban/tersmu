{ stdenv, fetchFromGitLab, pappy }:

stdenv.mkDerivation {
  name = "tersmu-src";

  src = ./..;
  # fetchFromGitLab {
  #   owner = "zugz";
  #   repo = "tersmu";
  #   rev = "577e4533c463a5e5001dd4e90c1a0f0199af6f4d";
  #   sha256 = "1x00rqfs2l85v83wjpk92p3qrg2rf49rdgvh81ngygabkb2rfjwk";
  # };

  postUnpackPhase = "rm Makefile";

  buildPhase = ''
    mkdir Pappy
    PAPPY=${pappy}/bin/pappy
    PAPPYOPTS="--2010 -e --monad"
    $PAPPY $PAPPYOPTS Lojban.pappy
    $PAPPY $PAPPYOPTS Morphology.pappy

    $PAPPY --2010 --write-files
  '';

  installPhase = ''
    cp -r . $out/
  '';
}
