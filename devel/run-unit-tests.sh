#!/usr/bin/env bash
# Compiles and runs tests/unit/; arguments go to hspec, e.g. `--match isEqual`.

set -euo pipefail

cd "$(dirname "$0")/.."

build="$(mktemp -d)"
trap 'rm -rf "$build"' EXIT

ghc -outputdir "$build/linter" -o "$build/jsonlinter" src/JSONLinter.hs
ghc -ilib -isrc -itests/unit -outputdir "$build/unit" -o "$build/unit-tests" tests/unit/Main.hs
JSON_LINTER="$build/jsonlinter" "$build/unit-tests" "$@"
