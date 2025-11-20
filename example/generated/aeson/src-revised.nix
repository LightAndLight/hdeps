{ stdenv }:
stdenv.mkDerivation {
  name = "hackage.haskell.org-aeson-2.2.3.0-r4-src";
  src = import ./src.nix;
  installPhase = ''
    mkdir $out
    cp -R * $out/
    cp ${import ./cabal.nix} $out/aeson.cabal
  '';
}
