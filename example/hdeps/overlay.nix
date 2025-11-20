self: super: {
  aeson = self.callPackage ./aeson {};
  beam-postgres-trans = self.callPackage ./beam-postgres-trans {};
  hdeps = self.callPackage ./hdeps {};
}
