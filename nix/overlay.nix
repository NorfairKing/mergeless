final: previous:
with final.lib;
with final.haskell.lib;
let
  mergelessPkg = name:
    doBenchmark (
      buildStrictly (
        final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
      )
    );
in
{
  mergelessPackages =
    {
      mergeless = mergelessPkg "mergeless";
      genvalidity-mergeless = mergelessPkg "genvalidity-mergeless";
      mergeless-persistent = mergelessPkg "mergeless-persistent";
    };

  mergelessRelease =
    final.symlinkJoin {
      name = "mergeless-release";
      paths = attrValues final.mergelessPackages;
    };

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          composeExtensions (old.overrides or (_: _: { })) (
            self: super: final.mergelessPackages
          );
      }
    );
}
