let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc928";
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [
    aeson
    aeson-pretty
    bytestring
    containers
    data-default-class
    lens-aeson
    req
    text
    transformers
    unordered-containers
    vector
  ];
}
