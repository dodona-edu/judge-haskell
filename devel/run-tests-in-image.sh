#!/usr/bin/env bash
# Runs devel/run-tests.sh as root in ${IMAGE:-ghcr.io/dodona-edu/dodona-haskell:latest}; UPDATE_EXPECTED=1 passes through.

set -euo pipefail

repo="$(cd "$(dirname "$0")/.." && pwd)"

docker run --rm --platform linux/amd64 --user root \
  ${UPDATE_EXPECTED:+--env UPDATE_EXPECTED} \
  --volume "$repo:/judge-haskell" \
  --workdir /judge-haskell \
  "${IMAGE:-ghcr.io/dodona-edu/dodona-haskell:latest}" \
  devel/run-tests.sh "$@"
