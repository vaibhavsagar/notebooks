let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc928 = sup.haskell.packages.ghc902.override {
          overrides = self: super: {
            bv-little = sel.haskell.lib.dontCheck super.bv-little;
          };
        };
      };
    };
  };
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc928";
  nixpkgs  = import pkgs.nixpkgs { overlays = [ overlay ]; config.allowBroken = true; };
  packages = self: with self; [ bv-little unordered-containers ];
}
