# shellcheck shell=bash

# shellcheck disable=SC2121
set unstable := true

default:
    just --list

format:
    #!/usr/bin/env bash
    # shellcheck disable=SC2034
    for i in {1..3}; do
        fourmolu -i src app test test-lib bench
    done
    cabal-fmt -i cardano-utxo-csmt.cabal
    nixfmt ./*.nix
    nixfmt nix/*.nix

hlint:
    #!/usr/bin/env bash
    hlint app src test test-lib bench

bench:
    #!/usr/bin/env bash
    cabal bench
unit match="":
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    if [[ '{{ match }}' == "" ]]; then
      cabal test unit-tests \
          --test-show-details=direct
    else
      cabal test unit-tests\
          --test-show-details=direct \
          --test-option=--match \
          --test-option="{{ match }}"
    fi

build:
    #!/usr/bin/env bash
    cabal build all --enable-tests --enable-benchmarks

CI:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just unit
    cabal-fmt -c cardano-utxo-csmt.cabal
    fourmolu -m check src app test test-lib
    hlint -c src app test test-lib


build-docker tag='latest':
    #!/usr/bin/env bash
    nix build .#docker-image
    docker load < result
    version=$(nix eval --raw .#version)
    docker image tag ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt:"$version" \
        "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt:{{ tag }}"

start-docker bg="false":
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    if [[ '{{ bg }}' == "true" ]]; then
        docker compose -f CD/docker-compose.yaml up -d --remove-orphans
    else
        docker compose -f CD/docker-compose.yaml up --remove-orphans
    fi

build-and-start-docker bg="false":
    #!/usr/bin/env bash
    just build-docker
    just start-docker "{{ bg }}"

logs-docker:
    #!/usr/bin/env bash
    docker compose -f CD/proxy/docker-compose.yaml logs -ft

stop-docker:
    #!/usr/bin/env bash
    docker compose -f CD/proxy/docker-compose.yaml down

push-docker tag='latest':
    #!/usr/bin/env bash
    docker push "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt:{{ tag }}"

release version arch:
    #!/usr/bin/env bash
    set -euo pipefail
    just build-docker
    just push-docker
    ./CI/release.sh "{{ version }}" "{{ arch }}"

integration match="":
    #!/usr/bin/env bash
    set -euo pipefail
    cabal test cardano-utxo-csmt-integration-test \
        --test-show-details=direct \
        --test-option=--match \
        --test-option="{{ match }}"

integration-all:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal test cardano-utxo-csmt-integration-test \
        --test-show-details=direct

# Utility to dump UTxO from a preprod cardano node socket and load it into the CSMT UTxO database
dump-and-load-utxo socket-path address:
    #!/usr/bin/env bash
    nix develop -c cardano-cli query utxo \
        --socket-path "{{ socket-path }}" \
        --testnet-magic 1 \
        --address "{{ address }}" \
        --output-cbor-bin > test/assets/utxo.bin
    nix shell -c cardano-utxo-csmt \
        -i test/assets/utxo.bin \
        -c test/assets/db \
        -n 5000 # depending on the address this is an arbitrary number