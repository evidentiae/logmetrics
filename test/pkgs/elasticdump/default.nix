{ lib, nodePackages, fetchFromGitHub }:

with lib;

let

  np = nodePackages.override {
    generated = ./generated.nix;
    self = np;
  };

in nodePackages.buildNodePackage rec {
  name = "elasticdump-${version}";

  version = "0.16.1";

  src = [ (fetchFromGitHub {
    owner = "taskrabbit";
    repo = "elasticsearch-dump";
    rev = "v${version}";
    sha256 = "1icxdzlazrb70ssqsmfy1mfhdbsvswa82ibpx4scr60vvvb2509m";
  }) ];

  deps = (filter (v: nixType v == "derivation") (attrValues np));

}
