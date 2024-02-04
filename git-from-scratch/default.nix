{ system ? builtins.currentSystem }:
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
  nixpkgs = (import pkgs.nixpkgs { inherit system; overlays = [ overlay ]; });
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
  cleanSource = name: type: let
      baseName = baseNameOf (toString name);
      lib = nixpkgs.lib;
    in !(
      (type == "directory" && (baseName == ".git" || baseName == "dist")) ||
      (type == "directory" && (baseName == "solarized"))                  ||
      (type == "symlink"   && (lib.hasPrefix "result" baseName))          ||
      lib.hasSuffix ".nix"   baseName                                     ||
      lib.hasSuffix ".yaml"  baseName                                     ||
      lib.hasSuffix ".hi"    baseName                                     ||
      lib.hasSuffix ".o"     baseName                                     ||
      lib.hasSuffix ".ipynb" baseName                                     ||
      lib.hasSuffix ".sock"  baseName
    );
  git-from-scratch =
    nixpkgs.haskellPackages.callCabal2nix "git-from-scratch" (builtins.filterSource cleanSource ./.) {};
in nixpkgs.callPackage "${pkgs.ihaskell}/nix/release.nix" { compiler = "ghc948"; }{
  packages = self: with self; [
    SHA
    attoparsec
    base16-bytestring
    byteable
    bytestring
    containers
    directory
    filepath
    git-from-scratch
    utf8-string
    zlib
  ];
  systemPackages = pkgs: with pkgs; [
    coreutils
    findutils
    git
    qpdf
    vim
  ];
  extraEnvironmentBinaries = [jupyterlab];
}
