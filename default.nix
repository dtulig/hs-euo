{ mkDerivation, base, mtl, stdenv, transformers }:
mkDerivation {
  pname = "hs-euo";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base mtl transformers ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/dtulig/hs-euo#readme";
  description = "Simple bindings and utilities to the EasyUO uo.dll.";
  license = stdenv.lib.licenses.mit;
}
