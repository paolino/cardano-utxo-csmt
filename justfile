# shellchec  shell=bash

# shellcheck disable=SC2121
set unstable := true

default:
    just --list

format:
    #!/usr/bin/env bash
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec fourmolu -i {} +
    cabal-fmt -i cardano-utxo-csmt.cabal
    nixfmt ./*.nix
    nixfmt nix/*.nix

hlint:
    #!/usr/bin/env bash
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec hlint {} +

bench:
    #!/usr/bin/env bash
    cabal bench
unit match="" *args='' :
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    if [[ '{{ match }}' == "" ]]; then
      cabal test unit-tests \
          --test-show-details=direct \
          --test-options="{{args}}"
    else
      cabal test unit-tests \
          --test-show-details=direct \
          --test-option=--match \
          --test-option="{{ match }}" \
          --test-options="{{ args }}"
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
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec fourmolu -m check {} +
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec hlint {} +
    just update-swagger
    git diff --exit-code docs/assets/swagger.json
    just update-diagrams
    git diff --exit-code docs/presentation/light-clients.md


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
    docker compose -f CD/docker-compose.yaml logs -ft

stop-docker:
    #!/usr/bin/env bash
    docker compose -f CD/docker-compose.yaml down

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

update-swagger:
    #!/usr/bin/env bash
    nix run .#cardano-utxo-swagger > docs/assets/swagger.json

# Run memory test for streaming UTxO extraction from Mithril snapshots
# Network: mainnet, preprod, preview (default: preprod)
# Shows RTS stats including maximum residency (peak memory)
mithril-extraction-memory-test network="preprod":
    #!/usr/bin/env bash
    set -euo pipefail
    tmp_dir=$(mktemp -d)
    trap 'rm -rf "$tmp_dir"' EXIT
    echo "Running memory test with {{ network }}..."
    echo "Tmp dir: $tmp_dir (cleaned up on exit)"
    echo "This will download a Mithril snapshot (~2-3GB for preprod)"
    echo ""
    cabal run memory-test -- \
        --network "{{ network }}" \
        --tmp-dir "$tmp_dir"

# Run Mithril E2E tests (requires mithril-client in PATH)
mithril-e2e:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal test unit-tests \
        --test-show-details=direct \
        --test-option=--match \
        --test-option="Mithril"

# Serve documentation locally (finds free port)
serve-docs:
    #!/usr/bin/env bash
    set -euo pipefail
    port=8000
    while nc -z localhost $port 2>/dev/null; do
        port=$((port + 1))
    done
    echo "Serving docs on http://localhost:$port"
    mkdocs serve -a "localhost:$port"

# Build presentation slides with Marp
slides:
    #!/usr/bin/env bash
    just update-diagrams
    marp --html docs/presentation/light-clients.md -o docs/presentation/index.html

# Generate mermaid.ink URLs from .mmd files and update slides
update-diagrams:
    #!/usr/bin/env bash
    set -euo pipefail
    cd docs/presentation
    for mmd in diagrams/*.mmd; do
        name=$(basename "$mmd" .mmd)
        # Generate URL-safe base64
        encoded=$(base64 -w0 < "$mmd" | tr '+/' '-_')
        url="https://mermaid.ink/svg/$encoded"
        echo "$name: $url"
        # Update the markdown - find img with matching diagram name in comment
        sed -i "s|<!-- $name --><img src=\"[^\"]*\"|<!-- $name --><img src=\"$url\"|g" light-clients.md
    done