let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc864 = sup.haskell.packages.ghc864.override {
          all-cabal-hashes = sel.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/09dd3a69af0d88e0a8df6552b2398185a8740abf.tar.gz";
            sha256 = "1xs6cc8zz5lff1bvzwn1sw076fislpxfk7zsyvwfp5hgl46bjhj6";
          };
          overrides = self: super: {
            sbv = self.callHackage "sbv" "8.3" {};
          };
        };
      };
    };
  };
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc864";
  nixpkgs  = import pkgs.nixpkgs { overlays = [ overlay ]; };
  packages = self: with self; [ sbv ];
  systemPackages = self: with self; [ z3 ];
}
