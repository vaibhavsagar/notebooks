{ system ? builtins.currentSystem }:
let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    nix-filter = import pkgs.nix-filter;
  };
  nixpkgs = (import pkgs.nixpkgs { inherit system; overlays = [ overlay ]; });
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
  hvega-src = builtins.fetchTarball {
    url = "https://github.com/vaibhavsagar/hvega/tarball/5dd4147d619305b0288b9831ddb805e049e85ea5";
    sha256 = "sha256:1zaqvc0id5xagfzvlzxr8xaa7k4n4xbmvi68p7xz91rr960chi1c";
  };
  ihaskell-hvega = nixpkgs.haskellPackages.callCabal2nix "ihaskell-hvega" "${hvega-src}/ihaskell-hvega" {};
in nixpkgs.callPackage "${pkgs.ihaskell}/nix/release.nix" { compiler = "ghc98"; }{
  extraEnvironmentBinaries = [ jupyterlab ];
  packages = self: with self; [ here ihaskell-hvega ];
}
