let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc8104";
  nixpkgs  = import pkgs.nixpkgs {};
  packages = self: with self; [
    aeson
    aeson-pretty
    bytestring
    data-default-class
    microlens
    microlens-aeson
    process
    req
    text
  ];
}
