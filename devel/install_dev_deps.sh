#!/usr/bin/env bash
# Installs the packages-dev.txt executables into cabal's installdir.

set -euo pipefail

cd "$(dirname "$0")/.."

mapfile -t packages < <(grep -Ehv '^[[:space:]]*(#|$)' packages-dev.txt)

cabal update
cabal install --overwrite-policy=always "${packages[@]}"
