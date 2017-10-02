let
  fetch    = (import <nixpkgs> {}).fetchFromGitHub;
  IHaskell = fetch {
    owner  = "gibiansky";
    repo   = "IHaskell";
    rev    = "8d022fa441fe8c7ebc9f377c0f30bee60fbde6b0";
    sha256 = "0wpvc7mw5x8gvclx1aziil4zn36w79dl6svfl69l7vnsc7y10ykk";
  };
  pinned   = fetch {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "83706dd49f4b476ae44b39f64d1fcdf587783c7a";
    sha256 = "18d8rcnhb70qqaqa5h2ganazn07a8mimpj29ilbzx0pi5a8zj7bv";
  };
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
  pkgs = nixpkgs;
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
