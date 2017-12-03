{ mkDerivation, base, comonad, containers, stdenv, text }:
mkDerivation {
  pname = "day1";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base comonad containers text ];
  license = stdenv.lib.licenses.bsd3;
}
