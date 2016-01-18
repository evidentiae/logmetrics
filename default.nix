{ mkDerivation, stdenv, aeson, base, bytestring
, containers, directory, filepath, http-client, http-types
, lens, scotty, text, transformers, wai, wai-extra
, warp, warp-tls, wreq
}:
mkDerivation {
  pname = "logmetrics";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers directory
    filepath http-client http-types lens scotty text
    transformers wai wai-extra warp warp-tls wreq
  ];
  license = stdenv.lib.licenses.unfree;
}
