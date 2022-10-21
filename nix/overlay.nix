final: prev:
with final.lib;
with final.haskell.lib;
{
  haskellPackages =
    prev.haskellPackages.override (
      old:
      {
        overrides =
          composeExtensions (old.overrides or (_: _: { })) (
            self: super:
              let
                mergelessPkg = name:
                  doBenchmark (
                    buildStrictly (
                      self.callPackage (../${name}/default.nix) { }
                    )
                  );
                mergelessPackages =
                  {
                    mergeless = mergelessPkg "mergeless";
                    genvalidity-mergeless = mergelessPkg "genvalidity-mergeless";
                    mergeless-persistent = mergelessPkg "mergeless-persistent";
                  };
              in
              {
                inherit mergelessPackages;
                mergelessRelease =
                  final.symlinkJoin {
                    name = "mergeless-release";
                    paths = attrValues final.haskellPackages.mergelessPackages;
                  };
              }
              // mergelessPackages
          );
      }
    );
}
