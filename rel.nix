{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "rel";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
