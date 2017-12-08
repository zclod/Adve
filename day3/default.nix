{ mkDerivation, attoparsec, base, containers, memoize, stdenv, text
}:
mkDerivation {
  pname = "day3";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base containers memoize text
  ];
  license = stdenv.lib.licenses.bsd3;
}
