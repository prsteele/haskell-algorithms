{
  description = "Common data structures and algorithms";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    systems.url = "github:nix-systems/default";
  };

  outputs = { self, nixpkgs, systems, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs (import systems);
      mkPkgs = system: import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };
    in
    {
      packages = forAllSystems
        (system:
          let
            pkgs = mkPkgs system;
          in
          {
            default = pkgs.haskellPackages.haskell-algorithms;

            test =
              let
                shell = self.devShells.${system}.default;
              in
              pkgs.writeShellApplication {
                name = "test";
                runtimeInputs = shell.buildInputs ++ shell.nativeBuildInputs;
                text = ''
                  cabal test --test-show-details=always "$@"
                '';
              };
          }
        );

      devShells = forAllSystems (system:
        let
          pkgs = mkPkgs system;
        in
        {
          default = pkgs.haskellPackages.shellFor {
            packages = hpkgs: [ hpkgs.haskell-algorithms ];

            buildInputs = [
              pkgs.haskellPackages.haskell-language-server
              pkgs.ormolu
              pkgs.cabal-install
            ];

            withHoogle = true;
          };
        }
      );

      overlays = {
        default = final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = hfinal: hprev: {
              haskell-algorithms = hfinal.callCabal2nix "haskell-algorithms" self { };
            };
          };
        };
      };
    };
}
