# treefmt.nix
{pkgs, ...}: {
  projectRootFile = "flake.nix";
  programs.alejandra.enable = true;
  programs.cabal-fmt.enable = true;
  programs.ormolu.enable = true;
}
