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
      formatter = treefmtEval.config.build.wrapper;
      overlay = self: super: {
        unbound-generics = pkgs.haskell.lib.unmarkBroken super.unbound-generics;
      };
      haskellPackages = pkgs.haskell.packages.ghc910;
      haskellPackages' = haskellPackages.extend overlay;
      packageName = "jsonnet";
    in {
      packages.${packageName} =
        haskellPackages'.callCabal2nix packageName self {};

      defaultPackage = self.packages.${system}.${packageName};

      devShell = pkgs.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
        buildInputs = with haskellPackages'; [
          haskell-language-server
          ghcid
          cabal-install
        ];
      };

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

      # for `nix fmt`
      formatter = treefmtEval.config.build.wrapper;
    });
}
