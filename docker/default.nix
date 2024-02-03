let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    nix-filter = import pkgs.nix-filter;
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc948 = sup.haskell.packages.ghc948.override {
          overrides = self: super: {
            ghc-syntax-highlighter = self.ghc-syntax-highlighter_0_0_10_0;
          };
        };
      };
    };
  };
  nixpkgs = (import pkgs.nixpkgs { overlays = [ overlay ]; });
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
  NB_USER = "jovyan";
  NB_UID = "1000";
  dockerEtc = nixpkgs.runCommand "docker-etc" {} ''
    mkdir -p $out/etc/pam.d

    echo "root:x:0:0::/root:/bin/sh" > $out/etc/passwd
    echo "jovyan:x:1000:1000::/home/jovyan:" >> $out/etc/passwd
    echo "root:!x:::::::" > $out/etc/shadow
    echo "jovyan:!:::::::" >> $out/etc/shadow

    echo "root:x:0:" > $out/etc/group
    echo "jovyan:x:1000:" >> $out/etc/group
    echo "root:x::" > $out/etc/gshadow
    echo "jovyan:!::" >> $out/etc/gshadow
  '';
  ihaskell = nixpkgs.callPackage "${pkgs.ihaskell}/nix/release.nix" { compiler = "ghc948"; }{
    packages = self: with self; [];
    extraEnvironmentBinaries = [jupyterlab];
    staticExecutable = true;
  };
  image = nixpkgs.dockerTools.buildLayeredImage {
    name = "ihaskell-nix";
    tag = "latest";
    contents =  [
      dockerEtc
      ihaskell
      nixpkgs.bashInteractive
    ];
    config = {
      Cmd = ["jupyter-notebook" "--ip=0.0.0.0"];
      User = NB_USER;
      WorkingDir = "/home/${NB_USER}";
    };
    fakeRootCommands = ''
      mkdir -m 1777 ./tmp
      mkdir -m 777 -p ./home/${NB_USER}
      chown -R ${NB_UID} ./home/${NB_USER}
    '';
  };
in image
