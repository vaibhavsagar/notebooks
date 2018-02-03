let
  fetch    = (import <nixpkgs> {}).fetchFromGitHub;
  # ./updater gibiansky IHaskell > ./ihaskell.json
  IHaskell = fetch (builtins.fromJSON (builtins.readFile ./versions.json)).ihaskell;
  # ./updater NixOS nixpkgs-channels nixos-17.09 > ./nixpkgs.json
  pinned   = fetch (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs;
  nixpkgs = import pinned {};
in import "${IHaskell}/release.nix" {
  inherit nixpkgs;
  packages = self: with self; [ transformers ];
}
