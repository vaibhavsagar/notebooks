let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc902";
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [ transformers ];
}
