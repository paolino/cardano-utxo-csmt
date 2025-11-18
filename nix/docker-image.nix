{ pkgs, project, version, ... }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt";
  tag = version;
  config = { EntryPoint = [ "cardano-utxo-csmt" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      project.packages.cardano-utxo-csmt.package.components.exes.cardano-utxo-csmt
    ];
  };
}
