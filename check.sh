#!/usr/bin/env sh
set -e

dune build aoc_2019/aoc_2019.exe

_build/default/aoc_2019/aoc_2019.exe all > test.out

if diff test.out test.exp > test.diff; then
    echo "Pass"
else
    echo "Failed, see test.diff"
fi
