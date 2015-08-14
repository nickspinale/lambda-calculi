{ mkDerivation, attoparsec, base, stdenv }:
mkDerivation {
  pname = "untyped-lambda-calculus";
  version = "0.1.0.0";
  src = ./src;
  buildDepends = [ attoparsec base ];
  license = stdenv.lib.licenses.mit;
}
