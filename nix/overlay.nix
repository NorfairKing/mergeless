final: previous:
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
  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions (old.overrides or (_: _: { })) (
            self: super: final.mergelessPackages
          );
      }
    );
}
