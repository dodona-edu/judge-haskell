#!/bin/bash
VALUE=$(cat)
SOURCE=$(jsawk  'return this.source' <<< "$VALUE")
RESOURCES=$(jsawk  'return this.resources' <<< "$VALUE")

touch "Input.hs"
echo "module Input where" >  "Input.hs"
cat "$SOURCE" >>  "Input.hs" 
cp  $(echo "$RESOURCES"/*_test.hs)  . 
OUTPUT="$(runghc *_test.hs 2> /dev/null)"

cat <<HERE
{
    "accepted": false,
    "description": "failed :-(",
    "status": "helaas onsuccesvol",
    "groups": [{
        "accepted": false,
        "description": "tab title",
        "groups": $OUTPUT    
     }]
}
HERE
