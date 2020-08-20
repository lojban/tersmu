{ mkDerivation, base, containers, mtl, process, stdenv, syb
, transformers, src-gen
}:

mkDerivation {
  pname = "tersmu";
  version = "0.2.2";
  src = src-gen.outPath;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base containers mtl process syb transformers
  ];
  executableHaskellDepends = [
    base containers mtl process syb transformers
  ];
  homepage = "https://mbays.sdf.org/tersmu";
  description = "A semantic parser for lojban";
  license = stdenv.lib.licenses.gpl3;
} // { meta.maintainer = (import ./nix/maintainers.nix).lboklin; }

