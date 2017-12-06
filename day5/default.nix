{ mkDerivation, attoparsec, base, containers, ListZipper, stdenv
, text
}:
mkDerivation {
  pname = "day5";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base containers ListZipper text
  ];
  license = stdenv.lib.licenses.bsd3;
}
