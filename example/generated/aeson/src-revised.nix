{ stdenv, callPackage }:
stdenv.mkDerivation {
  name = "hackage.haskell.org-aeson-2.2.3.0-r4-src";
  src = callPackage ./src.nix {};
  installPhase = ''
    mkdir $out
    cp -R * $out/
    cp ${callPackage ./cabal.nix {}} $out/aeson.cabal
  '';
}
