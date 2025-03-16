#!/usr/bin/env bash

set -eu

test="${1%.*}"

if dune exec ./opt.exe "$1" > "$test.ll"; then
    # Validate LLVM IR
    opt -S "$test.ll" > /dev/null
    # Validate transformations
    dune exec ./opt.exe -- -O "$1" > "$test.opt.ll"
    alive-tv "$test.ll" "$test.opt.ll"
fi
