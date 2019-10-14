let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc865 = sup.haskell.packages.ghc865.override {
          all-cabal-hashes = sel.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/c5887c4a21b6bf7e2c8f338d55ec098206c51d0c.tar.gz";
            sha256 = "1kvlyknysx7rqlsn9x765w8rckbb7rs6c9wr3vv6fazpjpdrg7dw";
          };
          overrides = self: super: {
            sbv = self.callHackage "sbv" "8.4" {};
          };
        };
      };
    };
  };
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc865";
  nixpkgs  = import pkgs.nixpkgs { overlays = [ overlay ]; };
  packages = self: with self; [ sbv ];
  systemPackages = self: with self; [ z3 ];
}
