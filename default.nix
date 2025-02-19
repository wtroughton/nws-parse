{ mkDerivation, base, lib, megaparsec, parser-combinators, tasty
, tasty-hunit, text, time
}:
mkDerivation {
  pname = "nws-parse";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base megaparsec parser-combinators text time
  ];
  testHaskellDepends = [
    base megaparsec tasty tasty-hunit text time
  ];
  description = "Parse NWS reports";
  license = lib.licenses.bsd3;
}
