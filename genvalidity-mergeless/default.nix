{ mkDerivation, autodocodec, autodocodec-yaml, base, containers
, criterion, genvalidity, genvalidity-containers
, genvalidity-criterion, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-time, genvalidity-uuid
, lib, mergeless, mtl, pretty-show, QuickCheck, random
, safe-coloured-text, sydtest, sydtest-discover, time, uuid
}:
mkDerivation {
  pname = "genvalidity-mergeless";
  version = "0.4.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers genvalidity genvalidity-containers genvalidity-time
    mergeless QuickCheck
  ];
  testHaskellDepends = [
    autodocodec autodocodec-yaml base containers genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-uuid mergeless mtl
    pretty-show QuickCheck random safe-coloured-text sydtest time uuid
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion genvalidity-criterion mergeless
  ];
  homepage = "https://github.com/NorfairKing/mergeless#readme";
  license = lib.licenses.mit;
}
