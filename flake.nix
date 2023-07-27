{
  description = "Jsonnet implementation in pure Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc962;

        overlay = self: super: {
          # Required for ghcid
          shelly = pkgs.haskell.lib.dontCheck super.shelly_1_12_1;
          ghcid = pkgs.haskell.lib.dontCheck super.ghcid;
        };

        haskellPackages' = haskellPackages.extend overlay;

        packageName = "jsonnet";
      in {
        packages.${packageName} =
          haskellPackages'.callCabal2nix packageName self {};

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages'; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
