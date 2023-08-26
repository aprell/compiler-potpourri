#!/usr/bin/env bash

set -u

mode=sequential
N=10

while [[ $# -gt 0 ]]; do
  case "$1" in
    -parallel|--parallel)
      mode=parallel
      shift
      ;;
    -*)
      echo "Unknown option '$1'"
      exit 1
      ;;
    *)
      N="$1"
      break
      ;;
  esac
done

if [ "$mode" = sequential ]; then
    for x in examples/*.hir; do
        echo "Fuzzing $x"
        ./mutate.sh "$x" "1-$N" > /dev/null
        ./fuzz.sh "$x"
    done
fi

if [ "$mode" = parallel ]; then
    parallel --jobs 8 --keep-order \
        "echo Fuzzing {};" \
        "./mutate.sh {} 1-$N > /dev/null;" \
        "./fuzz.sh {}" \
        ::: examples/*.hir
fi
