# treefmt.nix
{pkgs, ...}: {
  projectRootFile = "flake.nix";
  programs = {
    alejandra.enable = true;
    cabal-fmt.enable = true;
    ormolu.enable = true;
  };
  settings = {
    global.excludes = [
      ".github/**.yml"
      "test/golden/**"
      "**LICENSE"
      "*.jsonnet"
      "*.md"
      "*.yaml"
    ];
  };
}
