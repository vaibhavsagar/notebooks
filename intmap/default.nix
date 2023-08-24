let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc928";
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [ pretty-show ];
}
