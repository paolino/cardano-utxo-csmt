{ indexState, pkgs, asciinema, mkdocs, ... }:

let
  libOverlay = { lib, pkgs, ... }: { };

  shell = { pkgs, ... }: {
    tools = {
      cabal = { index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      hoogle = { index-state = indexState; };
      fourmolu = { index-state = indexState; };
      hlint = { index-state = indexState; };
      implicit-hie = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.gitAndTools.git
      pkgs.just
      pkgs.nixfmt-classic
      pkgs.shellcheck
      pkgs.mkdocs
      mkdocs.mkdocs-asciinema-player
      mkdocs.mkdocs-markdown-callouts
      asciinema.compress
      asciinema.resize
      pkgs.asciinema
    ];
    shellHook = ''
      echo "Entering shell for cardano-utxo-csmt CLI development"
    '';
  };

  fullyStaticOptions = { pkgs, ... }:
    let libs = with pkgs; [ zlib openssl libffi gmp6 ];
    in {
      enableShared = false;
      enableStatic = true;
      configureFlags = map (l: "--ghc-option=-optl=-L${l}/lib") (libs);
    };
  musl = { pkgs, ... }: {
    packages.cardano-utxo-csmt.components.exes.cardano-utxo-csmt =
      (fullyStaticOptions { inherit pkgs; });
    doHaddock = false;
  };
  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "cardano-utxo-csmt";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };
    modules = [ libOverlay ];
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.cardano-utxo-csmt =
    project.hsPkgs.cardano-utxo-csmt.components.exes.cardano-utxo-csmt;
  packages.bench = project.hsPkgs.cardano-utxo-csmt.components.benchmarks.bench;
  packages.unit-tests =
    project.hsPkgs.cardano-utxo-csmt.components.tests.unit-tests;
  musl64 = project.projectCross.musl64.hsPkgs;
}
