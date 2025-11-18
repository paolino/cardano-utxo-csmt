# shellcheck shell=bash

# shellcheck disable=SC2121
set unstable := true

format:
    #!/usr/bin/env bash
    # shellcheck disable=SC2034
    for i in {1..3}; do
        fourmolu -i src app test test-lib bench CI/rewrite-libs
    done
    cabal-fmt -i cardano-utxo-csmt.cabal CI/rewrite-libs/rewrite-libs.cabal
    nixfmt ./*.nix
    nixfmt nix/*.nix
    nixfmt CI/rewrite-libs/*.nix
    nixfmt CI/rewrite-libs/nix/*.nix

hlint:
    #!/usr/bin/env bash
    hlint app src test CI/rewrite-libs

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
    cabal build all --enable-tests

CI:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just unit
    cabal-fmt -c cardano-utxo-csmt.cabal CI/rewrite-libs/rewrite-libs.cabal
    fourmolu -m check src app test CI/rewrite-libs
    hlint -c src app test CI/rewrite-libs

build-linux:
    #!/usr/bin/env bash
    nix build .#linux64.tarball

build-macos:
    #!/usr/bin/env bash
    nix build .#macos64.tarball
build-docker tag='latest':
    #!/usr/bin/env bash
    nix build .#proxy-docker-image
    docker load < result
    version=$(nix eval --raw .#version)
    docker image tag ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-proxy:"$version" \
        "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-proxy:{{ tag }}"
    docker image tag ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-proxy:"$version" \
        "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-proxy:latest"
    nix build .#source-docker-image
    docker load < result
    docker image tag ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-source:"$version" \
        "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-source:{{ tag }}"
    docker image tag ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-source:"$version" \
        "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-source:latest"

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
    docker push "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-source:{{ tag }}"
    docker push "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-source:latest"
    docker push "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-proxy:{{ tag }}"
    docker push "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt-proxy:latest"

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