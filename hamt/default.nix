let
  pkgs = import ../pkgs.nix;
  nixpkgs  = import pkgs.nixpkgs {};
  ihaskell = import "${pkgs.ihaskell}/release.nix" {
    inherit nixpkgs;
    compiler = "ghc8104";
    packages = self: with self; [ bytestring memory pretty-show timeit vector here ihaskell-graphviz ];
  };
  customisedIHaskellExe = nixpkgs.haskell.lib.overrideCabal ihaskell.passthru.haskellPackages.ihaskell (_drv: {
    enableSharedExecutables = true;
  });
  customisedIHaskell = with ihaskell.passthru; ihaskellBuildEnvFunc {
    inherit ihaskellEnv jupyterlab;
    systemPackages = self: with self; [ graphviz ];
    ihaskellDataDir = let
      ihaskellGhcLib =  ihaskellGhcLibFunc customisedIHaskellExe ihaskellEnv;
      ihaskellKernelFile = ihaskellKernelFileFunc ihaskellGhcLib "-M3g -N2";
      ihaskellKernelSpec = ihaskellKernelSpecFunc ihaskellKernelFile;
    in ihaskellDataDirFunc ihaskellKernelSpec ihaskellLabextension;
  };
in customisedIHaskell
