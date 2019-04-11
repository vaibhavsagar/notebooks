let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc864";
  packages = self: with self; [];
}
