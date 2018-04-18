let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [ transformers ];
}
