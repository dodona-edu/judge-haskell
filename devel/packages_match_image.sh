#!/usr/bin/env bash
# Fails when packages.txt or .tool-versions disagree with the judge image this runs in.

set -euo pipefail

cd "$(dirname "$0")/.."

failed=0

check_tool() { # $1 = tool, $2 = its version in the image
    local pinned
    pinned="$(awk -v tool="$1" '$1 == tool { print $2 }' .tool-versions)"
    if [ "$pinned" != "$2" ]; then
        echo "MISMATCH .tool-versions pins $1 ${pinned:-(nothing)}, the image has $1 $2"
        failed=1
    fi
}

check_tool ghc "$(ghc --numeric-version)"
check_tool cabal "$(cabal --numeric-version)"

diff -u /packages.txt packages.txt || {
    echo "packages.txt must be identical to the image's /packages.txt, which is the source of truth."
    failed=1
}

[ "$failed" -eq 0 ] || exit 1
echo "packages.txt and .tool-versions match the image."
