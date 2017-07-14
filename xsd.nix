{ mkDerivation, attoparsec, base, case-insensitive, containers
, directory, doctest, filepath, lens, network-uri, parsers, pretty
, QuickCheck, quickcheck-instances, regex, stdenv, template-haskell
, text, time, xml-conduit, xml-lens
}:
mkDerivation {
  pname = "xsd";
  version = "0.5.0.1";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base case-insensitive containers lens network-uri
    parsers pretty regex template-haskell text time xml-conduit
    xml-lens
  ];
  testHaskellDepends = [
    base directory doctest filepath QuickCheck quickcheck-instances
    template-haskell
  ];
  homepage = "https://github.com/data61/xsd";
  description = "XML Schema data structures";
  license = stdenv.lib.licenses.bsd3;
}
