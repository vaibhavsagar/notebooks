let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release-8.6.nix" {
  packages = self: with self; [];
}
