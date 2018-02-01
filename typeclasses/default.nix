let
  fetch    = (import <nixpkgs> {}).fetchFromGitHub;
  # ./updater gibiansky IHaskell > ./ihaskell.json
  IHaskell = fetch (builtins.fromJSON (builtins.readFile ./versions.json)).ihaskell;
  # ./updater NixOS nixpkgs-channels nixos-17.09 > ./nixpkgs.json
  pinned   = fetch (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs;
  nixpkgs = import pinned {};
  cleanSource = name: type: let
      baseName = baseNameOf (toString name);
      lib = nixpkgs.lib;
    in !(
      (type == "directory" && (baseName == ".git" || baseName == "dist")) ||
      (type == "directory" && (baseName == "solarized"))                  ||
      (type == "symlink"   && (lib.hasPrefix "result" baseName))          ||
      lib.hasSuffix ".nix"   baseName                                     ||
      lib.hasSuffix ".yaml"  baseName                                     ||
      lib.hasSuffix ".hi"    baseName                                     ||
      lib.hasSuffix ".o"     baseName                                     ||
      lib.hasSuffix ".ipynb" baseName                                     ||
      lib.hasSuffix ".sock"  baseName
    );
in import "${IHaskell}/release.nix" {
  inherit nixpkgs;
  packages = self: with self; [];
}
