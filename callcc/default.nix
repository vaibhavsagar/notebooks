let
  pkgs     = import ../pkgs.nix;
  IHaskell = pkgs.ihaskell;
in import "${IHaskell}/release.nix" {
  nixpkgs = import pkgs.nixpkgs {};
}
