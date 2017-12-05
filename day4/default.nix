{ mkDerivation, attoparsec, base, containers, folds, stdenv, text
}:
mkDerivation {
  pname = "day4";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base containers folds text
  ];
  license = stdenv.lib.licenses.bsd3;
}
