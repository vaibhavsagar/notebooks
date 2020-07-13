let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc865";
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [ bv-little unordered-containers ];
}
