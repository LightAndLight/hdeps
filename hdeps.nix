{ mkDerivation, aeson, base, bytestring, containers, directory, lib
, optparse-applicative, process, text
}:
mkDerivation {
  pname = "hdeps";
  version = "0.1.0.0";
  src = ./.;
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
