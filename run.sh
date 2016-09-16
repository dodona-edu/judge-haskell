#!/bin/bash

input="$(cat | base64 | tr -d "\n")"
env="$(ls | base64 | tr -d "\n")"

cat <<HERE
{
    "accepted": false,
    "description": "failed :-(",
    "status": "helaas onsuccesvol",
    "messages": [{
        "format": "code",
        "description": "$input"
    },{
        "format": "code",
        "description": "$env"
    }],
    "groups": [{
        "accepted": false,
        "description": "tab title",
        "groups": [{
            "accepted": false,
            "groups": [{
                "accepted": false,
                "description": "ggd 12 3",
                "tests": [{
                    "accepted": false,
                    "generated": "4",
                    "expected": "3"
                }]
            }]
        },{
            "accepted": true,
            "groups": [{
                "accepted": true,
                "description": "ggd 12 4",
                "tests": [{
                    "accepted": true,
                    "generated": "4"
                }]
            }]
        }]
    }]
}
HERE

