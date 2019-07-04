let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/73392e79aa62e406683d6a732eb4f4101f4732be.tar.gz";
    sha256 = "049fq37sxdn5iis7ni407j2jsfrxbb41pgv5zawi02qj47f74az9";
  };
  ihaskell = builtins.fetchTarball {
    url = "https://github.com/gibiansky/IHaskell/archive/555a6501324762ea8f04eabb5091f4d00c3e74a0.tar.gz";
    sha256 = "1dxb5pynx6cfld6r16g23pj5xl4sp0zy46arp1nzc7gcgwn0dp5s";
  };
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc864 = sup.haskell.packages.ghc864.override {
          overrides = self: super: {
            hlint = self.callHackage "hlint" "2.1.14" {};
          };
        };
      };
    };
  };
in import "${ihaskell}/release.nix" {
  nixpkgs = import nixpkgs { config.allowBroken = true; overlays = [ overlay ]; };
  compiler = "ghc864";
  packages = self: with self; [ llvm-hs llvm-hs-pure_8_0_0 ];
  systemPackages = self: with self; [ llvm_8 ];
}
