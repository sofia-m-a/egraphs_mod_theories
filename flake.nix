{
  description = "egraphs-modulo-theories";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; };

        project = returnShellEnv:
          pkgs.haskell.packages.ghc912.developPackage {
            inherit returnShellEnv;
            name = "egraphs-modulo-theories";
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with pkgs.haskell.packages.ghc912; [
                  # Specify your build/dev dependencies here.
                  cabal-install
                  haskell-language-server
                  pkgs.just
                  # cabal-fmt
                  # ghcid
                  # ormolu
                  # pkgs.nixpkgs-fmt
                ]);
          };
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      }
    );
}
