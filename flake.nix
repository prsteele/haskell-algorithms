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

            format = pkgs.writeShellApplication {
              name = "format";
              runtimeInputs = [ pkgs.git pkgs.ormolu pkgs.findutils ];
              text = ''
                git ls-files -z '*.hs' | xargs -0 -I{} ormolu --mode inplace {}
              '';
            };

            check-formatting = pkgs.writeShellApplication {
              name = "format";
              runtimeInputs = [ pkgs.git pkgs.ormolu pkgs.findutils ];
              text = ''
                git ls-files -z '*.hs' | xargs -0 -I{} ormolu --mode check {}
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
            packages =
              let
                enableBench = pkgs.haskell.lib.compose.overrideCabal (drv: { doBenchmark = true; });
              in
              hpkgs: [ (enableBench hpkgs.haskell-algorithms) ];

            buildInputs = [
              pkgs.haskellPackages.haskell-language-server
              pkgs.ormolu
              pkgs.cabal-install
            ];

            withHoogle = true;
            doBenchmark = true;
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
