let
  pkgs = import ../pkgs.nix;
  nixpkgs = import pkgs.nixpkgs {};
  NB_USER = "jovyan";
  NB_UID = "1000";
  ihaskell = import "${pkgs.ihaskell}/release.nix" {
    inherit nixpkgs;
    compiler = "ghc864";
    packages = self: with self; [];
  };
  image = nixpkgs.dockerTools.buildImage {
    name = "ihaskell-nix";
    tag = "latest";
    contents =  [
      ihaskell
      nixpkgs.bashInteractive
    ];
    runAsRoot = ''
      #!${nixpkgs.runtimeShell}
      ${nixpkgs.dockerTools.shadowSetup}
      mkdir -m 1777 /tmp
      mkdir -p /home/${NB_USER}
      ${nixpkgs.busybox}/bin/adduser --disabled-password --gecos "Default user" --uid ${NB_UID} ${NB_USER}
      chown -R ${NB_UID} /home/${NB_USER}
    '';
    config = {
      Cmd = ["ihaskell-notebook" "--ip" "0.0.0.0"];
      User = NB_USER;
      WorkingDir = "/home/${NB_USER}";
    };
  };
in image
