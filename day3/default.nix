{ mkDerivation, attoparsec, base, comonad, containers, folds
, newtype, profunctors, stdenv, text
}:
mkDerivation {
  pname = "day2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base comonad containers folds newtype profunctors text
  ];
  license = stdenv.lib.licenses.bsd3;
}
