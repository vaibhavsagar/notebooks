{ system ? builtins.currentSystem }:
let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    nix-filter = import pkgs.nix-filter;
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc96 = sup.haskell.packages.ghc96.override {
          overrides = self: super: {
            sbv = sup.haskell.lib.dontCheck (self.callHackage "sbv" "10.2" {});
          };
        };
      };
    };
  };
  nixpkgs = (import pkgs.nixpkgs { inherit system; overlays = [ overlay ]; });
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
in nixpkgs.callPackage "${pkgs.ihaskell}/nix/release.nix" { compiler = "ghc96"; }{
  extraEnvironmentBinaries = [jupyterlab];
  packages = self: with self; [ sbv ];
  systemPackages = self: with self; [ z3 ];
}
