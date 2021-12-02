let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc8107";
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [ bytestring memory pretty-show timeit vector here ihaskell-graphviz ];
  systemPackages = self: with self; [ graphviz ];
}
