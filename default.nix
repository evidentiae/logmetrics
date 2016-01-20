{ mkDerivation, stdenv, aeson, base, bytestring
, unordered-containers, directory, filepath, http-client, http-types
, lens, scotty, text, transformers, wai, wai-extra
, warp, warp-tls, wreq, stm, string-conv
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
    string-conv
  ];
  license = stdenv.lib.licenses.free;
}
