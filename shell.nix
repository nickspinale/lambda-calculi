{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, stdenv }:
      mkDerivation {
        pname = "lambda-calculi";
        version = "0.1.0.0";
        src = ./src;
        buildDepends = [ attoparsec base ];
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
