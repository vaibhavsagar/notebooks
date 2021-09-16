let
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc844 = sup.haskell.packages.ghc844.override {
          overrides = self: super: {
            zeromq4-haskell = sel.haskell.lib.dontCheck super.zeromq4-haskell;
          };
        };
      };
    };
  };
  nixpkgs = import (fetcher {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a7e559a5504572008567383c3dc8e142fa7a8633";
    sha256 = "16j95q58kkc69lfgpjkj76gw5sx8rcxwi3civm0mlfaxxyw9gzp6";
  }) { overlays = [ overlay ]; };
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  inherit nixpkgs;
  compiler = "ghc844";
  packages = haskellPackages: [ haskellPackages.aeson ];
}
