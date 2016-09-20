#!/bin/bash

VALUE="$(cat)"
SOURCE="$(jq -r '.source' <<< "$VALUE")"
RESOURCES="$(jq -r '.resources' <<< "$VALUE")"
JUDGE="$(jq -r '.judge' <<< "$VALUE")"
WORKDIR="$(jq -r '.workdir' <<< "$VALUE")"

cd "$WORKDIR"

# Create the Input.hs module, containing the submitted code.
echo "module Input where" > "Input.hs"
cat "$SOURCE" >> "Input.hs" 

cat <<HERE
{
    "accepted": false,
    "description": "failed :-(",
    "status": "wrong",
    "groups": [{
HERE

cp "$JUDGE/HJudge.hs" .
find "$RESOURCES" -name '*_test.hs' | while read testfile; do
    cp "$testfile" .
    testfile="$(basename "$testfile")"
    testname="${testfile%_test.hs}"
    tabtitle="${testname//_/ }"
    output="$(cabal exec runghc "$testfile" 2> stderr.file)"

    cat <<HERE
    }, {
        "accepted": false,
        "description": "${tabtitle:-test}",
        "messages": [{
            "format": "code",
            "permission": "staff",
            "description": "$(base64 < stderr.file | tr -d '\n')"
        }],
        "groups": ${output:-[]}
HERE
done | tail -n +2

cat <<HERE
    }]
}
HERE
