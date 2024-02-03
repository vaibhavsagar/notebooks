let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    nix-filter = import pkgs.nix-filter;
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc948 = sup.haskell.packages.ghc948.override {
          overrides = self: super: {
            ghc-syntax-highlighter = self.ghc-syntax-highlighter_0_0_10_0;
            sbv = sup.haskell.lib.dontCheck (self.callHackage "sbv" "10.1" {});
          };
        };
      };
    };
  };
  nixpkgs = (import pkgs.nixpkgs { overlays = [ overlay ]; });
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
in nixpkgs.callPackage "${pkgs.ihaskell}/nix/release.nix" { compiler = "ghc948"; }{
  extraEnvironmentBinaries = [ jupyterlab ];
  packages = self: with self; [
    aeson
    aeson-pretty
    bytestring
    data-default-class
    microlens
    microlens-aeson
    process
    req
    text
  ];
}
