#!/usr/bin/env bash

set -eu

test="${1%.*}"

if dune exec ./opt.exe "$1" > "$test.ll"; then
    # Check LLVM IR
    llc < "$test.ll" > /dev/null
    # Validate transformations
    dune exec ./opt.exe -- -opt "$1" > "$test.opt.ll"
    alive-tv "$test.ll" "$test.opt.ll"
fi
