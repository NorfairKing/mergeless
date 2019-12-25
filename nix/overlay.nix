final: previous:
with final.haskell.lib;

{
  mergelessPackages =
    {
      mergeless =
        failOnAllWarnings (
          final.haskellPackages.callCabal2nix "mergeless" ( final.gitignoreSource ../mergeless ) {}
        );
      genvalidity-mergeless =
        failOnAllWarnings (
          final.haskellPackages.callCabal2nix "genvalidity-mergeless" ( final.gitignoreSource ../genvalidity-mergeless ) {}
        );
    };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions ( old.overrides or (_: _: {}) ) (
              self: super: final.mergelessPackages
            );
        }
    );
}
