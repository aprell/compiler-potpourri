#!/usr/bin/env bash
# ShellChecked

set -eu
set -o pipefail

for test in "${1:-examples}"/*; do
    OUTPUT="test/$(basename "${test%.*}").out"
    dune exec --no-print-directory test/expect.exe "$test" > "$OUTPUT.actual"
    if git diff --no-index "$OUTPUT.expect" "$OUTPUT.actual"; then
        rm "$OUTPUT.actual"
    else
        echo
        read -rp "Accept this output [y/n/q]? " -n1
        echo
        if [[ $REPLY =~ [Yy] ]]; then
            mv "$OUTPUT.actual" "$OUTPUT.expect"
        elif [[ $REPLY =~ [Qq] ]]; then
            exit 1
        fi
    fi
done
