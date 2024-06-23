{
  description = "Common datastructures and algorithms";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    systems.url = "github:nix-systems/default";
  };

  outputs = { self, nixpkgs, systems, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs (import systems);
    in
    {
      packages = forAllSystems
        (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            local = pkgs.haskellPackages.callCabal2nix "haskell-algorithms" self { };
          in
          {
            default = local;
            haskell-algorithms = local;

            test = pkgs.writeScriptBin "test"
              ''
                cabal test --test-show-details=always "$@"
              '';
          }
        );

      overlays = {
        default = final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = hfinal: hprev: {
              haskell-algorithms = hprev.callCabal2nix "haskell-algorithms" self { };
            };
          };
        };
      };

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.haskellPackages.shellFor {
            packages = ps: [ self.packages.${system}.default ];
            buildInputs = [
              pkgs.haskellPackages.haskell-language-server
              pkgs.ormolu
              pkgs.cabal-install
            ];
            withHoogle = true;
          };
        }
      );
    };
}
