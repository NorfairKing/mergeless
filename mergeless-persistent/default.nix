{ mkDerivation, base, containers, genvalidity
, genvalidity-mergeless, genvalidity-persistent
, genvalidity-sydtest, lib, mergeless, microlens, monad-logger, mtl
, path, path-io, persistent, persistent-sqlite, persistent-template
, QuickCheck, sydtest, sydtest-discover, text, validity
}:
mkDerivation {
  pname = "mergeless-persistent";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers mergeless microlens persistent
  ];
  testHaskellDepends = [
    base containers genvalidity genvalidity-mergeless
    genvalidity-persistent genvalidity-sydtest mergeless monad-logger
    mtl path path-io persistent persistent-sqlite persistent-template
    QuickCheck sydtest text validity
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/mergeless#readme";
  description = "Support for using mergeless from persistent-based databases";
  license = lib.licenses.mit;
}
