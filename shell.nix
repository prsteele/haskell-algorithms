{ sources ? import ./nix/sources.nix
, pkgsF ? import sources.nixpkgs
}:
let
  local = import ./default.nix { inherit sources pkgsF; };
  pkgs = local.pkgs;
in
pkgs.haskellPackages.shellFor {
  packages = p: [ local.haskell-algorithms
                ];
  buildInputs = [ pkgs.cabal-install
                  pkgs.cabal2nix
                  pkgs.haskell-language-server
                  pkgs.ormolu
                ];
  withHoogle = true;
}
