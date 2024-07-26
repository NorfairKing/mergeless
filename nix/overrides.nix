{ lib
, haskell
, symlinkJoin
, ...
}:
with lib;
with haskell.lib;
self: _:
let
  mergelessPkg = name: doBenchmark (buildStrictly (self.callPackage (../${name}) { }));
  mergelessPackages = {
    mergeless = mergelessPkg "mergeless";
    genvalidity-mergeless = mergelessPkg "genvalidity-mergeless";
    mergeless-persistent = mergelessPkg "mergeless-persistent";
  };
in
{
  inherit mergelessPackages;
  mergelessRelease =
    symlinkJoin {
      name = "mergeless-release";
      paths = attrValues self.mergelessPackages;
    };
} // mergelessPackages
