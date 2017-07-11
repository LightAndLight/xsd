{ mkDerivation, attoparsec, base, containers, directory, doctest
, filepath, lens, pretty, QuickCheck, quickcheck-instances, stdenv
, template-haskell, text, time, xml-conduit
}:
mkDerivation {
  pname = "xsd";
  version = "0.5.0.1";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base containers lens pretty template-haskell text time
    xml-conduit
  ];
  testHaskellDepends = [
    base directory doctest filepath QuickCheck quickcheck-instances
    template-haskell
  ];
  homepage = "https://github.com/data61/xsd";
  description = "XML Schema data structures";
  license = stdenv.lib.licenses.bsd3;
}
