pythonPackages: pythonPackages.buildPythonPackage rec {
  pname = "rise";
  version = "5.5.1";
  name = "${pname}-${version}";
  src = builtins.fetchurl {
    url = "https://files.pythonhosted.org/packages/source/r/${pname}/${name}.tar.gz";
    sha256 = "0lh046s6r9jjs0vd2hqn82prn25mhjajzw9byh5gy1ypf28m4gas";
  };
  propagatedBuildInputs = [ pythonPackages.notebook ];
}
