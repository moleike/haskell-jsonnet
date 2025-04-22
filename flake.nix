{
  description = "Jsonnet implementation in pure Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    treefmt-nix,
    pre-commit-hooks,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
      overlay = self: super: {
        unbound-generics = pkgs.haskell.lib.unmarkBroken super.unbound-generics;
        jsonnet = self.callCabal2nix "jsonnet" ./. {};
      };
      haskellPackages = pkgs.haskell.packages.ghc910.extend overlay;
    in rec {
      # nix build
      packages.default = haskellPackages.jsonnet;

      # nix develop
      devShell = pkgs.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
        buildInputs = with haskellPackages; [
          haskell-language-server
          ghcid
          cabal-install
        ];
      };

      # nix flake check
      checks = {
        formatting = treefmtEval.config.build.check self;
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            treefmt.package = formatter;
            treefmt.enable = true;
          };
        };
      };

      # nix fmt
      formatter = treefmtEval.config.build.wrapper;
    });
}
