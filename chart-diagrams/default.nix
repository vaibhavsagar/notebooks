let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc864 = sup.haskell.packages.ghc864.override {
          all-cabal-hashes = sel.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/c5887c4a21b6bf7e2c8f338d55ec098206c51d0c.tar.gz";
            sha256 = "1kvlyknysx7rqlsn9x765w8rckbb7rs6c9wr3vv6fazpjpdrg7dw";
          };
          overrides = self: super: {
            Chart = self.callHackage "Chart" "1.9.1" {};
            Chart-cairo = self.callHackage "Chart-cairo" "1.9.1" {};
          };
        };
      };
    };
  };
in import "${pkgs.ihaskell}/release.nix" {
  nixpkgs = import pkgs.nixpkgs { config.allowBroken = true; overlays = [ overlay ]; };
  compiler = "ghc864";
  packages = self: with self; [ ihaskell-aeson ihaskell-blaze ihaskell-charts ihaskell-diagrams ];
}
