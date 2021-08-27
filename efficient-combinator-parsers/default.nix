let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc8104";
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [ timeit ];
}
