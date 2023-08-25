let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc928 = sup.haskell.packages.ghc928.override {
          overrides = self: super: {
            sbv = sup.haskell.lib.dontCheck (self.callHackage "sbv" "10.1" {});
          };
        };
      };
    };
  };
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc928";
  nixpkgs  = import pkgs.nixpkgs { overlays = [ overlay ]; };
  packages = self: with self; [ sbv ];
  systemPackages = self: with self; [ z3 ];
}
