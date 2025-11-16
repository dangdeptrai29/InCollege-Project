#!/usr/bin/env bash
# Runs search regression cases (Input_10.txt through Input_19.txt)
# Resets data/users before each run so the diffs are deterministic.

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

if [ ! -x ./incollege ]; then
  echo "Compiling incollege binary..."
  cobc -x -o ./incollege src/InCollege.cob
fi

seed_profiles="$(mktemp)"
seed_users="$(mktemp)"
trap 'rm -f "$seed_profiles" "$seed_users"' EXIT

cp data/profiles.txt "$seed_profiles"
cp data/users.txt "$seed_users"

ids=(10 11 12 13 14 15 16 17 18 19)
all_passed=true
for id in "${ids[@]}"; do
  cp "$seed_profiles" data/profiles.txt
  cp "$seed_users" data/users.txt
  cp "tests/Input_${id}.txt" io/InCollege-Input.txt
  rm -f io/InCollege-Output.txt

  ./incollege >/tmp/incollege_case_${id}.log

  if diff -u "tests/Output_${id}.txt" io/InCollege-Output.txt > /tmp/diff_case_${id}.log; then
    echo "Case ${id}: PASS"
    rm -f /tmp/diff_case_${id}.log /tmp/incollege_case_${id}.log
  else
    echo "Case ${id}: FAIL"
    cat /tmp/diff_case_${id}.log
    all_passed=false
  fi
  echo
  rm -f io/InCollege-Output.txt
 done

cp "$seed_profiles" data/profiles.txt
cp "$seed_users" data/users.txt

if $all_passed; then
  echo "All search regression cases passed."
else
  echo "Search regression suite reported failures." >&2
  exit 1
fi
