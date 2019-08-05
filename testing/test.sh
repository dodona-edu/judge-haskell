#!/bin/bash

path_to_exercise="$1"

[ -z "$path_to_exercise" ] \
    && echo 'Provide a path to an exercise as first argument.' \
    && exit 1

if ! [[ "$path_to_exercise" = /* ]]; then
    path_to_exercise="$(pwd)/$path_to_exercise"
fi

mkdir workdir
mkdir builddir

[ -d "$path_to_exercise/workdir" ] \
    && find "$path_to_exercise/workdir/" -mindepth 1 -maxdepth 1 | xargs cp -r -t workdir

sed -i 's/cabal exec -- ghc/stack ghc --/' "../run"
sh "../run" <<HERE
{
    "resources": "$path_to_exercise/evaluation",
    "judge": "$(pwd)/..",
    "workdir": "$(pwd)/workdir",
    "builddir": "$(pwd)/builddir",
    "time_limit": 30,
    "memory_limit": 100000000,
    "source": "$path_to_exercise/solution/Input.hs",
    "helper": "$path_to_exercise/evaluation/Helper.hs"
}
HERE
sed -i 's/stack ghc --/cabal exec -- ghc/' "../run"

