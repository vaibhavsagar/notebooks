let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc844 = sup.haskell.packages.ghc844.override {
          overrides = self: super: {
            system-fileio = sel.haskell.lib.doJailbreak super.system-fileio;
            Chart = sel.haskell.lib.doJailbreak super.Chart;
            Chart-cairo = sel.haskell.lib.doJailbreak super.Chart-cairo;
          };
        };
      };
    };
  };
in import "${pkgs.ihaskell}/release.nix" {
  nixpkgs = import pkgs.nixpkgs { config.allowBroken = true; overlays = [ overlay ]; };
  compiler = "ghc844";
  packages = self: with self; [ ihaskell-aeson ihaskell-blaze ihaskell-charts ihaskell-diagrams ];
}
