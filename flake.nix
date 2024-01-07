{
  description = "Common datastructures and algorithms";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        package = "haskell-algorithms";
        local = haskellPackages.callCabal2nix package self { };
      in
        {
          packages.${package} = local;
          packages.default = self.packages.${system}.${package};
          defaultPackage = self.packages.${system}.default;
          devShells.default = pkgs.haskellPackages.shellFor {
            packages = p: [ local ];
            buildInputs = [
              pkgs.haskellPackages.haskell-language-server
              pkgs.ormolu
              pkgs.cabal-install
            ];
            withHoogle = true;
            doBenchmark = true;
          };
          devShell = self.devShells.${system}.default;
        }
    );
}
