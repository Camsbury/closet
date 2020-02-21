{ nixpkgs ? import <nixpkgs> {
              overlays = [];
            }
}:
nixpkgs.pkgs.haskellPackages.callCabal2nix "closet" ./. {}
