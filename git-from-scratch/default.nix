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
    rev    = "81628ce54f703fd518432ec3b429083cb183d747";
    sha256 = "01bir37cxwi252977prxgd0nmlzybcs4x7w2an51jim6mvk7500s";
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
  ];
}
