self: super: {
  aeson = self.callPackage ./hackage.haskell.org-aeson-2.2.3.0.nix {};
  beam-postgres-trans = self.callPackage ./github.com-LightAndLight-beam-3ebb79baff43416caee95e81c05ce74c429bbe82-beam-postgres-trans.nix {};
}
