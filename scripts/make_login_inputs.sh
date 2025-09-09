#!/usr/bin/env bash
set -euo pipefail

# Generate file-driven inputs for InCollege login flows.
# Writes to io/InCollege_Input.txt by default.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
cd "$ROOT_DIR"

OUT="io/InCollege_Input.txt"

usage() {
  cat <<USAGE
Usage:
  $0 success [USER] [PASS]
  $0 failure-4-then-success [USER] [PASS]

Examples:
  $0 success testuser 'Password1!'
  $0 failure-4-then-success testuser 'Password1!'
USAGE
}

mode=${1:-}
user=${2:-testuser}
pass=${3:-'Password1!'}

mkdir -p io

case "$mode" in
  success)
    printf '1\n%s\n%s\n' "$user" "$pass" > "$OUT"
    echo "[wrote] $OUT (success flow)";;
  failure-4-then-success)
    {
      printf '1\n'
      for i in 1 2 3 4; do printf 'wronguser%s\nbadpass%s\n' "$i" "$i"; done
      printf '%s\n%s\n' "$user" "$pass"
    } > "$OUT"
    echo "[wrote] $OUT (4 failures then success)";;
  *)
    usage; exit 2;;
esac

echo "[preview]"
nl -ba "$OUT"

