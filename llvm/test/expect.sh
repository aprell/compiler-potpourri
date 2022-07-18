#!/usr/bin/env bash
# ShellChecked

set -eu
set -o pipefail

if [ -z "$(command -v alive-tv)" ]; then
    echo "To use alive-tv, follow the instructions at"
    echo "https://github.com/AliveToolkit/alive2."
    exit 0
fi

for test in \
    fib pow fastpow sort test01 test02 test03 test04 test05 test06 test07
do
    OUTPUT="test/$test.out"
    dune exec test/expect.exe --      "$test" > test/src.ll
    dune exec test/expect.exe -- -opt "$test" > test/tgt.ll
    alive-tv test/src.ll test/tgt.ll > "$OUTPUT.actual"
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
