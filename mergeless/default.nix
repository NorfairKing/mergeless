{ mkDerivation, aeson, autodocodec, base, containers, deepseq, lib
, mtl, text, validity, validity-containers
}:
mkDerivation {
  pname = "mergeless";
  version = "0.4.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base containers deepseq mtl text validity
    validity-containers
  ];
  homepage = "https://github.com/NorfairKing/mergeless#readme";
  license = lib.licenses.mit;
}
