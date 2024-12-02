#!/usr/bin/env bash
# ShellChecked

set -eu
set -o pipefail

if [ -z "$(command -v qbe)" ]; then
    echo "To use qbe, follow the instructions at"
    echo "https://c9x.me/compile."
    exit 0
fi

for test in "${1:-examples}"/*; do
    OUTPUT="test/$(basename "${test%.*}").out"
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
