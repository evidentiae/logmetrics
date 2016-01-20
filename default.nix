{ mkDerivation, stdenv, aeson, base, bytestring
, unordered-containers, directory, filepath, http-client, http-types
, lens, scotty, text, transformers, wai, wai-extra
, warp, warp-tls, wreq, stm, string-conv, hashable, stm-containers
}:
mkDerivation {
  pname = "logmetrics";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring unordered-containers directory
    filepath http-client http-types lens scotty text
    transformers wai wai-extra warp warp-tls wreq stm
    string-conv hashable stm-containers
  ];
  license = stdenv.lib.licenses.free;
}
