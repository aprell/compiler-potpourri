#!/usr/bin/env bash
# ShellChecked

set -eu
set -o pipefail

for test in \
    fib pow fastpow sort test01 test02 test03 test04 test05 test06 test07
do
    OUTPUT="test/$test.out"
    dune exec test/expect.exe --      "$test" > test/01.ssa
    dune exec test/expect.exe -- -opt "$test" > test/02.ssa
    qbe test/01.ssa test/02.ssa > "$OUTPUT.actual"
    if git diff --no-index "$OUTPUT.expect" "$OUTPUT.actual"; then
        rm test/01.ssa test/02.ssa "$OUTPUT.actual"
    else
        echo
        read -rp "Accept this output [y/n/q]? " -n1
        echo
        if [[ $REPLY =~ [Yy] ]]; then
            rm test/01.ssa test/02.ssa
            mv "$OUTPUT.actual" "$OUTPUT.expect"
        elif [[ $REPLY =~ [Qq] ]]; then
            exit 1
        fi
    fi
done
