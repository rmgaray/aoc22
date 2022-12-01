{
  description = "Solutions to Advent of Code 2022";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = {
    self,
    nixpkgs,
    pre-commit-hooks,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages;
        ghcAttr = haskellPackages.ghc924;
        # Project's dependencies
        ghc = ghcAttr.ghcWithHoogle (ps:
          with ps; [
            base
            aeson
            ghc-lib-parser
            text
          ]);
        # Haskell tools
        devTools = with ghcAttr; [
          cabal-install
          haskell-language-server
          ormolu
        ];
      in {
        checks = {
          pre-commit-check =
            pre-commit-hooks.lib.${system}.run
            {
              src = ./.;
              hooks = {
                ormolu.enable = true;
                alejandra.enable = true;
              };
              settings = {
                ormolu.cabalDefaultExtensions = true;
              };
            };
        };
        devShell = pkgs.mkShell {
          name = "aoc-devshell";
          buildInputs = [ghc] ++ devTools;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };
      }
    );
}
