let IHaskell = (import <nixpkgs> {}).fetchFromGitHub {
    owner  = "gibiansky";
    repo   = "IHaskell";
    rev    = "f39b812fdcc5566210f9960f8218c5fc7fd40a77";
    sha256 = "1hz9rsgv3y4cmmlc7655m96jad953d6fv6cqvp8yjdv3sj7szjza";
  };
in import "${IHaskell}/release.nix" {
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
