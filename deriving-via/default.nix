let
  pkgs = import ../pkgs.nix;
  rise = pythonPackages: pythonPackages.buildPythonPackage rec {
    pname = "rise";
    version = "5.5.0";
    name = "${pname}-${version}";
    src = builtins.fetchurl {
      url = "https://files.pythonhosted.org/packages/source/r/rise/rise-5.5.0.tar.gz";
      sha256 = "0885xf5zaf0lj8cxvkwam61n6yfqasy6ri5zzyh6hvdj9pg70mwp";
    };
    propagatedBuildInputs = [ pythonPackages.notebook ];
  };
in import "${pkgs.ihaskell}/release.nix" {
  nixpkgs = import pkgs.nixpkgs {};
  compiler = "ghc864";
  packages = self: with self; [];
  pythonPackages = p: [ (rise p) ];
}
