{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (self: super: {
              haskellPackages = super.haskellPackages.extend (import ./hdeps/overlay.nix);
            })
          ];
        };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal-install

            (haskellPackages.ghcWithPackages (p: [p.aeson p.beam-postgres-trans]))
          ];
        };
      }
    );
}
