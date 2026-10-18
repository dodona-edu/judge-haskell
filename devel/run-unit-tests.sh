#!/usr/bin/env bash
# Compiles and runs tests/unit/; arguments go to hspec, e.g. `--match isEqual`.

set -euo pipefail

cd "$(dirname "$0")/.."

build="$(mktemp -d)"
trap 'rm -rf "$build"' EXIT

ghc -ilib -isrc -itests/unit -outputdir "$build/unit" -o "$build/unit-tests" tests/unit/Main.hs
"$build/unit-tests" "$@"
