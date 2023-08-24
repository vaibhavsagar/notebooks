let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  nixpkgs = import pkgs.nixpkgs {};
  compiler = "ghc928";
  packages = self: with self; [ ihaskell-aeson ihaskell-blaze ihaskell-charts ihaskell-diagrams ];
}
