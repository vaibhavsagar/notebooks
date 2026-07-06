{ system ? builtins.currentSystem }:
let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    nix-filter = import pkgs.nix-filter;
  };
  # ghc-lib-parser-9.12 is incompatible with GHC 9.10: github.com/digital-asset/ghc-lib/issues/620
  ghc910-overlay = import "${pkgs.ihaskell}/nix/overlay-9.10.nix";
  nixpkgs = (import pkgs.nixpkgs { inherit system; overlays = [ overlay ghc910-overlay ]; });
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
in nixpkgs.callPackage "${pkgs.ihaskell}/nix/release.nix" { compiler = "ghc910"; enableHlint = false; }{
  extraEnvironmentBinaries = [jupyterlab];
  packages = self: with self; [ containers transformers vector ];
}
