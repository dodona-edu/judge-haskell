#!/usr/bin/env bash
# Fails when packages.txt or .tool-versions disagree with the judge image this runs in.

set -euo pipefail

cd "$(dirname "$0")/.."

store="$(find /home/runner/.local/state/cabal/store -name package.db -print -quit)"
failed=0

mismatch() {
    echo "MISMATCH $1"
    failed=1
}

check_tool() { # $1 = tool, $2 = its version in the image
    local pinned
    pinned="$(awk -v tool="$1" '$1 == tool { print $2 }' .tool-versions)"
    [ "$pinned" = "$2" ] || mismatch ".tool-versions pins $1 ${pinned:-(nothing)}, the image has $1 $2"
}

check_tool ghc "$(ghc --numeric-version)"
check_tool cabal "$(cabal --numeric-version)"

while read -r line; do
    if [[ "$line" =~ ^(.+)-([0-9][0-9.]*)$ ]]; then
        name="${BASH_REMATCH[1]}"
        version="${BASH_REMATCH[2]}"
        installed="$(ghc-pkg --global --package-db "$store" field "$name" version --simple-output 2> /dev/null | paste -sd ' ' || true)"
        if [ -z "$installed" ]; then
            mismatch "packages.txt pins $name-$version, the image does not have $name"
        elif [[ " $installed " != *" $version "* ]]; then
            mismatch "packages.txt pins $name-$version, the image has $name $installed"
        fi
    else
        mismatch "packages.txt lists $line, which is not in <package>-<version> form"
    fi
done < <(grep -Ev '^[[:space:]]*(#|$)' packages.txt)

if [ "$failed" -ne 0 ]; then
    echo "packages.txt and .tool-versions mirror docker-images' dodona-haskell.dockerfile: update the stale side."
    exit 1
fi
echo "packages.txt and .tool-versions match the image."
