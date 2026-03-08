#!/usr/bin/env bash
# ShellChecked

set -eu
set -o pipefail

if [ -z "$(command -v alive-tv)" ]; then
    echo "To use alive-tv, follow the instructions at"
    echo "https://github.com/AliveToolkit/alive2."
    exit 0
fi

# See https://github.com/aprell/compiler-potpourri/issues/31
declare -A alive_tv_options=(
    [test01.hir]="--disable-undef-input"
    [test03.hir]="--src-unroll=10 --tgt-unroll=10"
)

for test in "${1:-examples}"/*; do
    OUTPUT="test/$(basename "${test%.*}").out"
    dune exec test/expect.exe --    "$test" > test/src.ll
    dune exec test/expect.exe -- -O "$test" > test/tgt.ll
    alive-tv ${alive_tv_options["$(basename "$test")"]:-} test/src.ll test/tgt.ll > "$OUTPUT.actual"
    if git diff --no-index "$OUTPUT.expect" "$OUTPUT.actual"; then
        rm test/src.ll test/tgt.ll "$OUTPUT.actual"
    else
        echo
        read -rp "Accept this output [y/n/q]? " -n1
        echo
        if [[ $REPLY =~ [Yy] ]]; then
            rm test/src.ll test/tgt.ll
            mv "$OUTPUT.actual" "$OUTPUT.expect"
        elif [[ $REPLY =~ [Qq] ]]; then
            exit 1
        fi
    fi
done
