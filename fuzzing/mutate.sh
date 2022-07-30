#!/usr/bin/env bash

set -u

repeat() {
    printf "%.0s$2" $(seq 1 "$1")
}

print_header() {
    len=${#1}
    printf "\n+"
    repeat $((len + 2)) "-"
    printf "+\n| %s |\n+" "$1"
    repeat $((len + 2)) "-"
    printf "+\n"
}

r="${2:-1-3}"
i="${r%-*}"
n="${r#*-}"
mutations=()

#src="$(basename "$1")"
src="$1"
name=${src%.*}
ext=${src##*.}

while [ $i -le "$n" ]; do
    tgt="corpus/$(basename "$name")_$(printf "%03d" $i).${ext}"
    radamsa "$src" -o "$tgt"
    if dune exec ./parse.exe "$tgt" 2> /dev/null; then
        mutations+=("$tgt")
        ((i++))
    fi
done

for mut in "${mutations[@]}"; do
    print_header "$mut"
    git diff --no-index --no-prefix "$src" "$mut"
done
