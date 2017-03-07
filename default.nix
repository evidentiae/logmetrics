{ mkDerivation, stdenv, aeson, base, extra, bytestring
, unordered-containers, lens, systemd, warp, scotty, text, transformers
, wreq, stm, string-conv, hashable, list-t, stm-containers, time
, zlib, pipes-bytestring, pipes-zlib, pipes-http, scientific
}:

let

  hsLib = import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { pkgs = <nixpkgs/pkgs>; };

  pipes-zlib' = hsLib.doJailbreak pipes-zlib;

in

mkDerivation {
  pname = "logmetrics";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base extra bytestring unordered-containers
    lens warp systemd scotty text transformers wreq stm
    string-conv hashable list-t stm-containers time
    zlib pipes-bytestring pipes-zlib' pipes-http
    scientific
  ];
  license = stdenv.lib.licenses.mit;
}
