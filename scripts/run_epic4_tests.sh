#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="/mnt/c/CEN 4020/InCollege-Project"
BIN="$REPO_ROOT/InCollegeTest"
IO_DIR="$REPO_ROOT/io"

INPUT_DIR="$REPO_ROOT/tests/epic4_inputs"
EXPECTED_DIR="$REPO_ROOT/tests/epic4_expected"

pass=0; fail=0

for tinput in "$INPUT_DIR"/*.txt; do
  tname=$(basename "$tinput" .txt)
  echo "== Running $tname =="

  mkdir -p "$IO_DIR"
  cp "$tinput" "$IO_DIR/InCollege-Input.txt"

  if ! $BIN > /dev/null; then
    echo "   Program crashed."
  fi

  exp="$EXPECTED_DIR/$tname.out.txt"
  got="$IO_DIR/InCollege-Output.txt"

  if diff -u --strip-trailing-cr "$exp" "$got"; then
    echo "   ✅ PASS"
    pass=$((pass+1))
  else
    echo "   ❌ FAIL"
    fail=$((fail+1))
  fi
done

echo "Summary: $pass passed, $fail failed"
