let
  pkgs = import ../pkgs.nix;
  rise = import ../rise.nix;
in import "${pkgs.ihaskell}/release.nix" {
  nixpkgs = import pkgs.nixpkgs {};
  compiler = "ghc902";
  packages = self: with self; [];
  pythonPackages = p: [ (rise p) ];
}
