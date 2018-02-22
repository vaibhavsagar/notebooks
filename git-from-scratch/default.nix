let
  fetch    = (import <nixpkgs> {}).fetchFromGitHub;
  # ./updater versions.json ihaskell
  IHaskell = fetch (builtins.fromJSON (builtins.readFile ../versions.json)).ihaskell;
  # ./updater versions.json nixpkgs nixos-17.09
  pinned   = fetch (builtins.fromJSON (builtins.readFile ../versions.json)).nixpkgs;
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
  git-from-scratch =
    nixpkgs.haskellPackages.callCabal2nix "git-from-scratch" (builtins.filterSource cleanSource ./.) {};
in import "${IHaskell}/release.nix" {
  inherit nixpkgs;
  packages = self: with self; [
    SHA
    attoparsec
    base16-bytestring
    byteable
    bytestring
    containers
    directory
    filepath
    git-from-scratch
    utf8-string
    zlib
  ];
  systemPackages = pkgs: with pkgs; [
    coreutils
    findutils
    git
    qpdf
    vim
  ];
}
