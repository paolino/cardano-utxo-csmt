{ CHaP, indexState, pkgs, mkdocs, asciinema, cardano-cli, mithril-client, ... }:

let
  indexTool = { index-state = indexState; };
  fix-libs = { lib, pkgs, ... }: {
    packages.cardano-crypto-praos.components.library.pkgconfig =
      lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    packages.cardano-crypto-class.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]];
  };
  shell = { pkgs, ... }: {
    tools = {
      cabal = indexTool;
      cabal-fmt = indexTool;
      haskell-language-server = indexTool;
      hoogle = indexTool;
      fourmolu = indexTool;
      hlint = indexTool;
      implicit-hie = indexTool;
    };
    withHoogle = true;
    buildInputs = [
      pkgs.just
      pkgs.nixfmt-classic
      pkgs.shellcheck
      pkgs.mkdocs
      pkgs.marp-cli
      mkdocs.from-nixpkgs
      mkdocs.asciinema-plugin
      mkdocs.markdown-callouts
      asciinema.compress
      asciinema.resize
      pkgs.asciinema
      cardano-cli."cardano-cli:exe:cardano-cli"
    ];
    shellHook = ''
      echo "Entering shell for cardano-utxo-csmt CLI development"
    '';
  };

  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "cardano-utxo-csmt";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };
    modules = [ fix-libs ];
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

  # Minimal E2E shell for testing with mithril-client
  e2eShell = project.shellFor {
    tools = { cabal = indexTool; };
    buildInputs = [ mithril-client pkgs.just ];
    withHoogle = pkgs.lib.mkForce false;
    shellHook = pkgs.lib.mkForce ''
      echo "E2E shell: mithril-client $(mithril-client --version)"
    '';
  };

in {
  devShells.default = project.shell;
  devShells.e2e = e2eShell;
  inherit project;
  packages.cardano-utxo-csmt =
    project.hsPkgs.cardano-utxo-csmt.components.exes.cardano-utxo-csmt;
  packages.bench = project.hsPkgs.cardano-utxo-csmt.components.benchmarks.bench;
  packages.unit-tests =
    project.hsPkgs.cardano-utxo-csmt.components.tests.unit-tests;
  packages.cardano-utxo-chainsync =
    project.hsPkgs.cardano-utxo-csmt.components.exes.cardano-utxo-chainsync;
  packages.cardano-utxo-swagger =
    project.hsPkgs.cardano-utxo-csmt.components.exes.cardano-utxo-csmt-swagger;
}
