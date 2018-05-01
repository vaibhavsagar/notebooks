let
  pkgs = import ../pkgs.nix;
  overlay = self: super: {
    all-cabal-hashes = self.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/5ed06084aa1933c8131bb674b5de86bbb977c5f0.tar.gz";
      sha256 = "06nva1c2wj7z8pagchlc5ax3rhb9mgc9myxh9k07p9vyi7s10zrk";
    };
  };
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc822";
  nixpkgs  = import pkgs.nixpkgs { overlays = [ overlay ]; };
  packages = self: with self; [ (callHackage "sbv" "7.7" {}) ];
  systemPackages = self: with self; [ z3 ];
}
