#!/usr/bin/env bash

set -u

N=${1:-10}

for x in examples/*.hir; do
    echo "Fuzzing $x"
    ./mutate.sh "$x" "1-$N" > /dev/null
    ./fuzz.sh "$x"
done
