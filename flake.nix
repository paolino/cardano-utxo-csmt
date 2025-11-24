{
  description = "CSMT UTxO, UTxO tracking via compact sparse merkle trees";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs = { follows = "haskellNix/nixpkgs-unstable"; };
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
    asciinema.url = "github:paolino/dev-assets?dir=asciinema";
  };

  outputs =
    inputs@{ self, nixpkgs, flake-utils, haskellNix, mkdocs, asciinema, ... }:
    let
      lib = nixpkgs.lib;
      version = self.dirtyShortRev or self.shortRev;

      perSystem = system:
        let
          pkgs = import nixpkgs {
            overlays = [
              haskellNix.overlay # some functions
            ];
            inherit system;
          };
          project = import ./nix/project.nix {
            indexState = "2025-08-07T00:00:00Z";
            inherit pkgs;
            mkdocs = mkdocs.packages.${system};
            asciinema = asciinema.packages.${system};
          };

          docker-image = import ./nix/docker-image.nix {
            inherit pkgs;
            inherit version;
            inherit project;
          };
          docker.packages = { inherit docker-image; };
          info.packages = { inherit version; };
          fullPackages = lib.mergeAttrsList [
            project.packages
            info.packages
            docker.packages
          ];

        in {

          packages = fullPackages // {
            default = fullPackages.cardano-utxo-csmt;
          };
          inherit (project) devShells;
        };

    in flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] perSystem;
}
