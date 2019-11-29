pythonPackages: pythonPackages.buildPythonPackage rec {
  pname = "rise";
  version = "5.6.0";
  name = "${pname}-${version}";
  src = builtins.fetchurl {
    url = "https://files.pythonhosted.org/packages/source/r/${pname}/${name}.tar.gz";
    sha256 = "09lfcm2zdi5k11af5c5nx4bnx2vr36z90skw0jp3mri7pqymrr1b";
  };
  propagatedBuildInputs = [ pythonPackages.notebook ];
}
