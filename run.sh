#!/bin/bash
VALUE=$(cat)
SOURCE=$(jsawk  'return this.source' <<< "$VALUE")
RESOURCES=$(jsawk  'return this.resources' <<< "$VALUE")

touch "Input.hs"
echo "module Input where" >  "Input.hs"
cat "$SOURCE" >>  "Input.hs" 
cp  $(echo "$RESOURCES"/*_test.hs)  . 
output="$(runghc *_test.hs 2> stderr.file)"
errput="$(cat stderr.file | base64 | tr -d '\n')"

cat <<HERE
{
    "accepted": false,
    "description": "failed :-(",
    "status": "wrong",
    "messages": [{
        "description": "$errput",
        "format": "code"
    }]
    "groups": [{
        "accepted": false,
        "description": "tab title",
        "groups": ${output:-[]}
     }]
}
HERE
