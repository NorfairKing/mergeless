final: previous:
with final.haskell.lib;
let
  mergelessPkg = name:
    doBenchmark (
      failOnAllWarnings (
        final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) {}
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
            final.lib.composeExtensions (old.overrides or (_: _: {})) (
              self: super: final.mergelessPackages
            );
        }
    );
}
