#!/bin/bash

input="$(cat | base64)"

cat <<HERE
{
    "groups": [{
        "description": "tab title",
        "messages": [{
            "format": "code",
            "description": "$input"
        }],
        "groups": [{
            "accepted": false,
            "groups": [{
                "accepted": false,
                "description": "ggd 12 3",
                "generated": "4",
                "expected": "3"
            }]
        },{
            "accepted": true,
            "groups": [{
                "accepted": true,
                "description": "ggd 12 4",
                "generated": "4"
            }]
        }]
    }]
}
HERE

