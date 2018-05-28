let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc822";
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [ (callHackage "sbv" "7.7" {}) ];
  systemPackages = self: with self; [ z3 ];
}
