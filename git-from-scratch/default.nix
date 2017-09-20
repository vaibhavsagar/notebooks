let
  fetch    = (import <nixpkgs> {}).fetchFromGitHub;
  IHaskell = fetch {
    owner  = "gibiansky";
    repo   = "IHaskell";
    rev    = "95735f19e40e6fe9c26f65642019bf23b074e84a";
    sha256 = "0zwvm40gbcbpqvdqilwc333dgam24p1hz60wnslk1awzbis554g1";
  };
  pinned   = fetch {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "83706dd49f4b476ae44b39f64d1fcdf587783c7a";
    sha256 = "18d8rcnhb70qqaqa5h2ganazn07a8mimpj29ilbzx0pi5a8zj7bv";
  };
in import "${IHaskell}/release.nix" {
  pkgs = import pinned {};
  packages = self: with self; [
    SHA
    attoparsec
    base16-bytestring
    byteable
    bytestring
    containers
    directory
    filepath
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
