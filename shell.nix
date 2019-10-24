{ pkgs ? (import <nixpkgs> {}) }:

let haskellPackages = pkgs.haskell.packages.ghc864; in

(haskellPackages.callPackage ./default.nix {}).env
