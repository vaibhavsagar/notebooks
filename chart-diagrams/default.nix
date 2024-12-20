{ system ? builtins.currentSystem }:
let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    nix-filter = import pkgs.nix-filter;
  };
  nixpkgs = (import pkgs.nixpkgs { inherit system; overlays = [ overlay ]; });
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
in nixpkgs.callPackage "${pkgs.ihaskell}/nix/release.nix" { compiler = "ghc96"; }{
  extraEnvironmentBinaries = [jupyterlab];
  packages = self: with self; [ ihaskell-aeson ihaskell-blaze ihaskell-charts ihaskell-diagrams ];
  systemPackages = self: with self; [ graphviz ];
}
