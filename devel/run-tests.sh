#!/usr/bin/env bash
# Runs the e2e suite as root in the judge image, as CI does; arguments go to tests/run-exercises.sh.

set -euo pipefail

cd "$(dirname "$0")/.."

tests/run-exercises.sh "$@"
