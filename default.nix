{ mkDerivation, stdenv, aeson, base, extra, bytestring, deepseq
, unordered-containers, lens, scotty, text, transformers
, wreq, stm, string-conv, hashable, list-t, stm-containers, time
}:
mkDerivation {
  pname = "logmetrics";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base extra bytestring deepseq unordered-containers
    lens scotty text transformers wreq stm
    string-conv hashable list-t stm-containers time
  ];
  license = stdenv.lib.licenses.free;
}
