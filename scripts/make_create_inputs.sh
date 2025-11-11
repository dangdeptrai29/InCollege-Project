#!/usr/bin/env bash
set -euo pipefail

# Generate file-driven inputs for InCollege Create Account flows.
# Writes to io/InCollege-Input.txt by default.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
cd "$ROOT_DIR"

OUT="io/InCollege-Input.txt"

usage() {
  cat <<USAGE
Usage:
  $0 success [USER] [PASS]
  $0 duplicate-then-success [EXISTING_USER] [NEW_USER] [PASS]
  $0 invalid-passwords-then-success [USER]
  $0 capacity [USER] [PASS]

Examples:
  $0 success newuser 'Password1!'
  $0 duplicate-then-success testuser newuser 'Password1!'
  $0 invalid-passwords-then-success userx
  $0 capacity extra 'Password1!'
USAGE
}

mode=${1:-}

mkdir -p io

case "$mode" in
  success)
    user=${2:-newuser}
    pass=${3:-'Password1!'}
    printf '2\n%s\n%s\n' "$user" "$pass" > "$OUT"
    echo "[wrote] $OUT (create success)";;
  duplicate-then-success)
    existing=${2:-testuser}
    newuser=${3:-newuser}
    pass=${4:-'Password1!'}
    printf '2\n%s\n%s\n%s\n' "$existing" "$newuser" "$pass" > "$OUT"
    echo "[wrote] $OUT (duplicate then success)";;
  invalid-passwords-then-success)
    user=${2:-userx}
    {
      printf '2\n%s\n' "$user"
      printf 'short\n'          # too short
      printf 'NoDigit!\n'      # no digit
      printf 'noupper1!\n'     # no uppercase
      printf 'NoSpecial1\n'    # no special
      printf 'ValidPass1!\n'   # valid
    } > "$OUT"
    echo "[wrote] $OUT (invalid passwords then success)";;
  capacity)
    user=${2:-extra}
    pass=${3:-'Password1!'}
    printf '2\n%s\n%s\n' "$user" "$pass" > "$OUT"
    echo "[wrote] $OUT (capacity reached flow)";;
  *)
    usage; exit 2;;
esac

echo "[preview]"
nl -ba "$OUT"
