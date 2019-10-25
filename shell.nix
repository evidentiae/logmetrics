{ pkgs ? (import <nixpkgs> {}) }:

let

  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = (self: hsuper: {
      stm-hamt = pkgs.haskell.lib.doJailbreak hsuper.stm-hamt;
    });
  });

in

(haskellPackages.callPackage ./default.nix {}).env
