{
  description = "Tersmu flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-20.03";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      pappy = pkgs.callPackage ./nix/pappy.nix {};
      src-gen = pkgs.callPackage ./nix/src-gen.nix { inherit pappy; };
      tersmu = pkgs.haskellPackages.callPackage ./default.nix { inherit src-gen; };

    in {
      packages.x86_64-linux = { inherit pappy tersmu; };
      defaultPackage.x86_64-linux = tersmu;
      apps.x86_64-linux = { inherit pappy tersmu; };
      app.x86_64-linux = tersmu;
    };
}
