{ sources ? import ./nix/sources.nix
, pkgsF ? import sources.nixpkgs
}:
let
  # Disable tests for these packages
  dontCheck = [
  ];

  # Disable Haddocks for these packages
  dontHaddock = [
  ];

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        let
          haskell-algorithms = haskellPackagesNew: haskellPackagesOld: {
            haskell-algorithms = haskellPackagesNew.callCabal2nix "haskell-algorithms" ./. { };
          };

          generatedOverrides = haskellPackagesNew: haskellPackagesOld:
            let
              toPackage = file: _: {
                name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
                value = haskellPackagesNew.callPackage (./. + "/nix/haskell/${file}") { };
              };
            in
              pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix/haskell);

          makeOverrides =
            function: names: haskellPackagesNew: haskellPackagesOld:
            let
              toPackage = name: {
                inherit name;
                value = function haskellPackagesOld.${name};
              };
            in
              builtins.listToAttrs (map toPackage names);

          composeExtensionsList =
            pkgs.lib.fold pkgs.lib.composeExtensions (_:_: {});

          # Manual overrides for packages
          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
          };
        in
          pkgs.haskellPackages.override {
            overrides = composeExtensionsList [
              haskell-algorithms
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck dontCheck)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddock)
              manualOverrides
            ];
          };
    };
  };

  pkgs = pkgsF { inherit config; };

in
{ haskell-algorithms = pkgs.haskellPackages.haskell-algorithms;
  pkgs = pkgs;
}
