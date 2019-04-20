let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release-8.6.nix" {
  nixpkgs = import pkgs.nixpkgs {};
  compiler = "ghc864";
  packages = self: with self; [];
}
