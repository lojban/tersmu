let pkgs = import <nixpkgs> {};
    pappy = pkgs.callPackage ./pappy.nix {};
    src-gen = pkgs.callPackage ./src-gen.nix { inherit pappy; };
 in pkgs.haskellPackages.callPackage ../default.nix { inherit src-gen; }
