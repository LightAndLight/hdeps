{ mkDerivation, aeson, base, bytestring, callPackage, containers
, directory, lib, optparse-applicative, process, text
}:
mkDerivation {
  pname = "hdeps";
  version = "0.1.0.0";
  src = callPackage ./src.nix {};
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    aeson base bytestring containers directory optparse-applicative
    process text
  ];
  license = lib.licenses.gpl3Plus;
  mainProgram = "hdeps";
}
