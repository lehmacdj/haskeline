#!/usr/bin/env bash
# Run haskeline's tests (stack does not run the tests of an extra-dep).
# See VENDORED.md for the two that fail on macOS.
set -euo pipefail
cd "$(dirname "$0")"
out="${TMPDIR:-/tmp}/haskeline-tests"
mkdir -p "$out"
# builds haskeline-examples-Test, the program the tests drive
stack build haskeline
stack exec --package HUnit -- \
  ghc -O0 -itests -outputdir "$out" -o "$out/unit" tests/Unit.hs
# stack exec puts haskeline-examples-Test on PATH
stack exec -- "$out/unit"
