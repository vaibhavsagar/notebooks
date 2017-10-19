let
  fetch    = (import <nixpkgs> {}).fetchFromGitHub;
  IHaskell = fetch {
    owner  = "gibiansky";
    repo   = "IHaskell";
    rev    = "32aea170c41b181e34e76b9d9eb391e540fed6e1";
    sha256 = "17n24pxizifvsxyh4lc8jgr45csbmrz936qssj8i5l96yr2fqz4b";
  };
  pinned   = fetch {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "c99239bca08d12bf98000961912b4c0ad52a8a7e";
    sha256 = "1d3hwaflsyb6vj2czj3jpaxvdmsr448sd0536lhaillvsm087y0g";
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
