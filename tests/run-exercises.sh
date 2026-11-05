#!/usr/bin/env bash
# Runs the judge on every tests/exercises submission as root in the image and diffs it with expected/ (usage: README).

set -euo pipefail
shopt -s nullglob dotglob

repo="$(cd "$(dirname "$0")/.." && pwd)"
filter="${1:-}"
judge_timeout=300

if [ ! -w /mnt ]; then
    echo "/mnt is not writable: run as root, like CI" >&2
    exit 2
fi

mapfile -t ignored < <(grep -v -e '^#' -e '^[[:space:]]*$' "$repo/.dodonaignore")
for pattern in "${ignored[@]}"; do
    if [[ ! "$pattern" =~ ^/[^/*?[]+/?$ ]]; then
        echo "unsupported .dodonaignore pattern (only /name and /name/): $pattern" >&2
        exit 2
    fi
done

is_ignored() { # $1 = top-level entry of the repo
    local name pattern
    name="$(basename "$1")"
    case "$name" in .git|.claude|.ghc.environment.*|dist-newstyle) return 0 ;; esac
    for pattern in "${ignored[@]}"; do
        [ "$pattern" = "/$name" ] && return 0
        [ "$pattern" = "/$name/" ] && [ -d "$1" ] && return 0
    done
    return 1
}

copy_judge() { # $1 = destination
    local entry
    mkdir "$1"
    for entry in "$repo"/*; do
        is_ignored "$entry" || cp -a "$entry" "$1/"
    done
}

# Rewritten in the raw output, because jq 1.5's gsub garbles strings with non-ASCII characters.
normalisations=(
    # builddir and the temp files run creates with mktemp
    -e 's|/dev/shm/tmp\.[A-Za-z0-9]+|/dev/shm/TMP|g'
    -e 's|/tmp/tmp\.[A-Za-z0-9]+|/tmp/TMP|g'
    # QuickCheck draws random values, so the number of tries and shrinks varies
    -e 's/after [0-9]+ tests?( and [0-9]+ shrinks?)?/after N tests and M shrinks/g'
    # the GHC runtime under amd64 emulation (Docker on arm64) reports interrupted timers
    -e '/^[^ :]+: Ticker: poll failed: /d'
    -e 's/[a-z]+: Ticker: poll failed: Interrupted system call: Interrupted system call\\n//g'
)

run_case() { # $1 = exercise dir, $2 = case name, $3 = file to write the result to
    local exercise="$1" case="$2" result="$3"
    local root language overrides exit_code output_key
    root="$(mktemp -d /mnt/XXXXXXXXXX)"
    copy_judge "$root/judge"
    cp -a "$exercise/evaluation" "$root/resources"
    mkdir "$root/submission"
    cp "$exercise/submissions/$case.hs" "$root/submission/source"
    if [ -d "$exercise/workdir" ]; then
        cp -a "$exercise/workdir" "$root/workdir"
    else
        mkdir "$root/workdir"
    fi
    chown -R runner:runner "$root"

    language="nl"
    [[ "$case" == *.en ]] && language="en"
    overrides='{}'
    [ -f "$exercise/config.json" ] && overrides="$(cat "$exercise/config.json")"
    jq -n --arg root "$root" --arg language "$language" --argjson overrides "$overrides" '
        {time_limit: 30, memory_limit: 536870912, programming_language: "haskell", natural_language: $language}
        + $overrides
        + {judge: "\($root)/judge", resources: "\($root)/resources", source: "\($root)/submission/source", workdir: "\($root)/workdir"}
    ' > "$root/config.json"

    exit_code=0
    (cd "$root/workdir" && timeout --kill-after=10 "$judge_timeout" \
        runuser -u runner -- env HOME=/home/runner /main.sh "$root/judge/run" \
        < "$root/config.json" > "$root/stdout" 2> "$root/stderr") || exit_code=$?
    pkill -KILL -u runner || true

    sed -i -E -e "s|$root|/mnt/SANDBOX|g" "${normalisations[@]}" "$root/stdout" "$root/stderr"
    jq -Rs . < "$root/stderr" > "$root/stderr.json"
    output_key=feedback
    if ! jq -s . < "$root/stdout" > "$root/output.json" 2> /dev/null; then
        output_key=raw_output
        jq -Rs . < "$root/stdout" > "$root/output.json"
    fi
    jq -S -n --argjson exit_code "$exit_code" --arg key "$output_key" \
        --slurpfile stderr "$root/stderr.json" --slurpfile output "$root/output.json" \
        '{exit_code: $exit_code, stderr: $stderr[0], ($key): $output[0]}' > "$result"
    rm -rf "$root"
}

ghc_version="$(ghc --numeric-version | cut -d. -f1,2)"
passed=0
failed=0
updated=0
for submission in "$repo"/tests/exercises/*/submissions/*.hs; do
    exercise="$(dirname "$(dirname "$submission")")"
    case="$(basename "$submission" .hs)"
    name="$(basename "$exercise")/$case"
    [[ "$name" == *"$filter"* ]] || continue

    expected="$exercise/expected/$case.json"
    variant="$exercise/expected/$case.ghc-$ghc_version.json"
    [ -f "$variant" ] && expected="$variant"
    actual="$(mktemp)"
    run_case "$exercise" "$case" "$actual"
    if [ "${UPDATE_EXPECTED:-}" = 1 ]; then
        mkdir -p "$(dirname "$expected")"
        cat "$actual" > "$expected"
        echo "updated $name"
        updated=$((updated + 1))
    elif [ ! -f "$expected" ]; then
        echo "FAIL $name: no expected output, run with UPDATE_EXPECTED=1 to create it"
        failed=$((failed + 1))
    elif diff -u --label "${expected#"$repo/"}" --label actual "$expected" "$actual"; then
        echo "ok   $name"
        passed=$((passed + 1))
    else
        echo "FAIL $name"
        failed=$((failed + 1))
    fi
    rm -f "$actual"
done

echo "$passed passed, $failed failed, $updated updated"
[ "$failed" -eq 0 ]
