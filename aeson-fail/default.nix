let
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  nixpkgs = import (fetcher {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "75f4ba05c63be3f147bcc2f7bd4ba1f029cedcb1";
    sha256 = "157c64220lf825ll4c0cxsdwg7cxqdx4z559fdp7kpz0g6p8fhhr";
  }) { overlays = [ ]; config.allowBroken = true; };
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  inherit nixpkgs;
  compiler = "ghc865";
  packages = haskellPackages: [ haskellPackages.aeson ];
}
