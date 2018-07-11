let
  pkgs = import ../pkgs.nix;
  overlay = self: super: {
    all-cabal-hashes = super.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/532d9c821a36f220c928be40465a6ace52bc3818.tar.gz";
      sha256 = "1yqn87r75cdf45wkbfa5vqxvsaxqsmypwjl4pw5w1g8qfrdilr18";
    };
  };
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc822";
  nixpkgs  = import pkgs.nixpkgs { overlays = [ overlay ]; };
  packages = self: with self; [ (callHackage "sbv" "7.9" {}) ];
  systemPackages = self: with self; [ z3 ];
}
