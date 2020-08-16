let pkgs = import <nixpkgs> {};
    pappy = pkgs.callPackage ./nix/pappy.nix {};
    src-gen = pkgs.callPackage ./nix/src-gen.nix { inherit pappy; };
 in pkgs.haskellPackages.callPackage ./default.nix { inherit src-gen; }
