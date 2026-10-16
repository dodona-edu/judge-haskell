#!/usr/bin/env bash
# Creates the GHC package environment from packages.txt plus any list files given, solved together.

set -euo pipefail

cd "$(dirname "$0")/.."

mapfile -t packages < <(grep -Ehv '^[[:space:]]*(#|$)' packages.txt "$@")

rm -f .ghc.environment.*
cabal update
cabal install --lib --package-env . "${packages[@]}"
