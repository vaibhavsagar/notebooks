{ system ? builtins.currentSystem }:
let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    nix-filter = import pkgs.nix-filter;
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc910 = sup.haskell.packages.ghc910.override {
          overrides = self: super: {
            # overly restrictive upper bound on `containers`
            mono-traversable-keys = sup.haskell.lib.doJailbreak super.mono-traversable-keys;
          };
        };
      };
    };
  };
  # ghc-lib-parser-9.12 is incompatible with GHC 9.10: github.com/digital-asset/ghc-lib/issues/620
  ghc910-overlay = import "${pkgs.ihaskell}/nix/overlay-9.10.nix";
  nixpkgs = (import pkgs.nixpkgs { inherit system; overlays = [ ghc910-overlay overlay ]; config.allowBroken = true; });
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
in nixpkgs.callPackage "${pkgs.ihaskell}/nix/release.nix" { compiler = "ghc910"; enableHlint = false; }{
  extraEnvironmentBinaries = [ jupyterlab ];
  packages = self: with self; [ bv-little unordered-containers ];
}
