#!/bin/bash
VALUE=$(cat)
SOURCE=$(jq  '.source' <<< "$VALUE")
RESOURCES=$(jq  '.resources' <<< "$VALUE")
JUDGE=$(jq  '.judge' <<< "$VALUE")

touch "Input.hs"
echo "module Input where" >  "Input.hs"
cat "$SOURCE" >>  "Input.hs" 

cat <<HERE
{
    "accepted": false,
    "description": "failed :-(",
    "status": "wrong",
    "messages": [{
        "format": "code",
        "description": "$(echo "$RESOURCES" | base64 | tr -d '\n')"
    }],
    "groups": [{
HERE

find "$RESOURCES" -name '*_test.hs' | while read testfile; do
    #cp  $(echo "$RESOURCES"/*_test.hs)  . 
    testname="${testfile%_test.hs}"
    tabtitle="${testname//_/ }"
    output="$(runghc -i.:"$RESOURCES" "$testfile" 2> stderr.file)"
    errput="$(cat stderr.file | base64 | tr -d '\n')"

    cat <<HERE
    }, {
        "accepted": false,
        "description": "${tabtitle:-test}",
        "messages": [{
            "description": "$errput",
            "format": "code"
        }],
        "groups": ${output:-[]}
HERE
done | tail -n +2

cat <<HERE
    }]
}
HERE
