let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc864";
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [ containers transformers vector ];
}
