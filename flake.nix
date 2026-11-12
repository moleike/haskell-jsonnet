{
  description = "jsonnet implementation in pure haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let

      pkgs = nixpkgs.legacyPackages.${system};

      hspkgs = pkgs.haskellPackages;

      cabalOnly = pkgs.lib.sourceFilesBySuffices ./. [ ".cabal" ];

      jsonnetPkg = hspkgs.callCabal2nix "jsonnet" cabalOnly {};

      jsonnet = jsonnetPkg.overrideAttrs (oldAttrs: {
        src = pkgs.lib.cleanSource ./.;
      });

      ghc = hspkgs.ghcWithHoogle (p: jsonnet.buildInputs);
      # wasmPkgs = ghc-wasm-meta.packages.${system};
    in {

      packages.default = jsonnet;

      devShells.default = pkgs.mkShell {
        inputsFrom = [ self.packages.${system}.default ];

        nativeBuildInputs = [
          ghc
          hspkgs.cabal-install
          hspkgs.haskell-language-server
          # wasmPkgs.wasmtime
        ];

        shellHook = ''
          export HOOGLE_DATABASE="${ghc}/share/doc/hoogle/default.hoo"
        '';
      };
    });
}
