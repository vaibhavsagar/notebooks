let
  fetch    = (import <nixpkgs> {}).fetchFromGitHub;
  # ./updater versions.json ihaskell
  IHaskell = fetch (builtins.fromJSON (builtins.readFile ../versions.json)).ihaskell;
  # ./updater versions.json nixpkgs nixos-17.09
  pinned   = fetch (builtins.fromJSON (builtins.readFile ../versions.json)).nixpkgs;
  nixpkgs = import pinned {};
in import "${IHaskell}/release.nix" {
  inherit nixpkgs;
}
