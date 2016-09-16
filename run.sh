#!/bin/bash

cat <<HERE
{
    "groups": [{
        "description": "tab title",
        "messages": [{
            "format": "code",
            "content": "asdf\\nasdf\\nasdf"
        }],
        "groups": [{
            "accepted": false,
            "groups": [{
                "accepted": false,
                "groups": [{
                    "accepted": false,
                    "decription": "ggd 12 3",
                    "generated": "4",
                    "expected": "3"
                }]
            }]
        },{
            "accepted": true,
            "groups": [{
                "accepted": true,
                "groups": [{
                    "accepted": true,
                    "decription": "ggd 12 4",
                    "generated": "4"
                }]
            }]
        }]
    }]
}
HERE

