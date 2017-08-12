let
  fetch    = (import <nixpkgs> {}).fetchFromGitHub;
  IHaskell = fetch {
    owner  = "gibiansky";
    repo   = "IHaskell";
    rev    = "f34c3e258934cb3c9ae5b0012fd7f669b0870e70";
    sha256 = "0rcy2yvi0qg443mfwri74d567smrz9v2cc9qs230caiz162aaih9";
  };
  pinned   = fetch {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "81628ce54f703fd518432ec3b429083cb183d747";
    sha256 = "01bir37cxwi252977prxgd0nmlzybcs4x7w2an51jim6mvk7500s";
  };
in import "${IHaskell}/release.nix" {
  nixpkgs = pinned;
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
