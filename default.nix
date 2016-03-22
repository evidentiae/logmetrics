{ mkDerivation, stdenv, aeson, base, extra, bytestring
, unordered-containers, lens, systemd, warp, scotty, text, transformers
, wreq, stm, string-conv, hashable, list-t, stm-containers, time
, zlib, pipes-bytestring, pipes-zlib, pipes-http
}:
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
    zlib pipes-bytestring pipes-zlib pipes-http
  ];
  license = stdenv.lib.licenses.mit;
}
