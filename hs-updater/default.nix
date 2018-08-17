let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc822";
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [
    aeson
    aeson-pretty
    bytestring
    data-default-class
    lens
    lens-aeson
    process
    req
    text
  ];
}
