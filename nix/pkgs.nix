let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay =
    import (
      (
        pkgs.fetchFromGitHub (import ./validity-version.nix)
        + "/nix/overlay.nix"
      )
    );
in
pkgsv {
  overlays =
    [
      validity-overlay
      (import ./overlay.nix)
      (import ./gitignore-src.nix)
    ];
}
