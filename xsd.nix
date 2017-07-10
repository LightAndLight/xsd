{ mkDerivation, attoparsec, base, directory, doctest, filepath
, lens, QuickCheck, quickcheck-instances, stdenv, template-haskell
, text, time
}:
mkDerivation {
  pname = "xsd";
  version = "0.5.0.1";
  src = ./.;
  libraryHaskellDepends = [ attoparsec base lens text time ];
  testHaskellDepends = [
    base directory doctest filepath QuickCheck quickcheck-instances
    template-haskell
  ];
  homepage = "https://github.com/data61/xsd";
  description = "XML Schema data structures";
  license = stdenv.lib.licenses.bsd3;
}
