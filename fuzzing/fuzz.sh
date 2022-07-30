#!/usr/bin/env bash

set -eu

test="$(basename ${1%.*})"

for t in "corpus/${test}"_*.hir; do
    ./check.sh "$t" && echo "[PASS] $t" || echo "[FAIL] $t"
    rm -f "${t%.*}."{ll,opt.ll}
done > "$test.fuzz.out" 2>&1

! grep FAIL "$test.fuzz.out"
