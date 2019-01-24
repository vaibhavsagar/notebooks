let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc844 = sup.haskell.packages.ghc844.override {
          overrides = self: super: {
            crackNum = self.callCabal2nix "crackNum" (sel.fetchFromGitHub {
              owner = "LeventErkok";
              repo = "crackNum";
              rev = "54cf70861a921062db762b3c50e933e73446c3b2";
              sha256 = "02cg64rq8xk7x53ziidljyv3gsshdpgbzy7h03r869gj02l7bxwa";
            }) {};
            sbv = self.callCabal2nix "sbv" (sel.fetchFromGitHub {
              owner = "LeventErkok";
              repo = "sbv";
              rev = "a2f71a07fa7896bebcc2f91e20eeb00162138e7d";
              sha256 = "0b6a9lj3ch1iji6kia7jpri87v2jnk68chpshxv54wzld93c2qvb";
            }) {};
          };
        };
      };
    };
  };
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc844";
  nixpkgs  = import pkgs.nixpkgs { overlays = [ overlay ]; };
  packages = self: with self; [ sbv ];
  systemPackages = self: with self; [ z3 ];
}
