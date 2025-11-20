{ mkDerivation, aeson, attoparsec, base, beam-core, beam-migrate
, beam-postgres, bytestring, callPackage, case-insensitive, conduit
, free, hashable, haskell-src-exts, hedgehog, lib, lifted-base
, monad-control, mtl, network-uri, postgresql-libpq
, postgresql-simple, scientific, tagged, tasty, tasty-hunit, text
, time, tmp-postgres, transformers-base, unordered-containers, uuid
, uuid-types, vector
}:
mkDerivation {
  pname = "beam-postgres-trans";
  version = "0.5.3.1";
  src = callPackage ./src.nix {};
  postUnpack = "sourceRoot+=/beam-postgres-trans; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base beam-core beam-migrate beam-postgres
    bytestring case-insensitive conduit free hashable haskell-src-exts
    lifted-base monad-control mtl network-uri postgresql-libpq
    postgresql-simple scientific tagged text time transformers-base
    unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    aeson base beam-core beam-migrate beam-postgres bytestring hedgehog
    postgresql-simple tasty tasty-hunit text tmp-postgres uuid vector
  ];
  doCheck = false;
  homepage = "https://haskell-beam.github.io/beam/user-guide/backends/beam-postgres";
  description = "Connection layer between beam and postgres";
  license = lib.licenses.mit;
}
