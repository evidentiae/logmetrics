{ pkgs ? import <nixpkgs> {} }:

let

  eval = module: pkgs.lib.evalModules {
    modules = [
      module
      { config._module.args = { inherit pkgs; }; }
    ];
  };

in {

  simple = {
    inherit ((eval ./simple.nix).config.libvirt.test.out) out;
  };

}
