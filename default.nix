{ mkDerivation, base, bytestring, hedis, http-types, lib, SHA, text
, wai, wai-extra, warp
}:
mkDerivation {
  pname = "go-shortener";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring hedis http-types SHA text wai wai-extra warp
  ];
  license = lib.licenses.mit;
  mainProgram = "go-shortener";
}
