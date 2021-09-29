let
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  aeson-src = builtins.fetchTarball {
    url = "https://github.com/haskell/aeson/tarball/16e912d70bc15f17b9f8f918c99bfa0e54725916";
    sha256 = "04hczzn3vzpwf0v32fkl3gm71pmdw4hcqfc7mphyich79d4y1bqk";
  };
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc865 = sup.haskell.packages.ghc865.override {
          overrides = self: super: {
            aeson_1_4_7_1 = (self.callCabal2nix "aeson" aeson-src {}).overrideScope (self: super: {
              Diff = self.callCabal2nix "Diff" ./Diff {};
            });
          };
        };
      };
    };
  };
  nixpkgs = import (fetcher {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "75f4ba05c63be3f147bcc2f7bd4ba1f029cedcb1";
    sha256 = "157c64220lf825ll4c0cxsdwg7cxqdx4z559fdp7kpz0g6p8fhhr";
  }) { overlays = [ overlay ]; config.allowBroken = true; };
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  inherit nixpkgs;
  compiler = "ghc865";
  packages = haskellPackages: [ haskellPackages.aeson_1_4_7_1 ];
}