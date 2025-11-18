{ pkgs, version, project, ... }:
let
  cardano-utxo-csmt =
    project.musl64.cardano-utxo-csmt.components.exes.cardano-utxo-csmt;
  tarball-derivation = pkgs.stdenv.mkDerivation rec {
    pname = "cardano-utxo-csmt";
    inherit version;
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${cardano-utxo-csmt}/bin/cardano-utxo-csmt $out/unpacked
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-linux64.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in { packages.linux64.tarball = tarball-derivation; }
