#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
cd "$ROOT_DIR"

BIN="./incollege"
SRC="src/InCollege.cob"
INPUT="io/InCollege-Input.txt"
OUTPUT="io/InCollege-Output.txt"
USERS="data/users.txt"

FAIL_MSG="Incorrect username/password, please try again."
SUCCESS_MSG="You have successfully logged in."
WELCOME_USER_MSG() { printf 'Welcome, %s' "$1"; }

# --- helpers ---------------------------------------------------------------

normalize_stream() {
  # Trim trailing CRs and trailing horizontal whitespace on every line
  sed -e 's/\r\+$//' -e 's/[[:space:]]\+$//'
}

print_header() {
  echo "[$1] $2"
}

check_mirror_matches() {
  # $1: console_raw
  local console_raw="$1"
  # Normalize both and compare; show diff only if mismatch
  local tmp_console tmp_file
  tmp_console="$(mktemp)"; tmp_file="$(mktemp)"
  printf "%s\n" "$console_raw" | normalize_stream > "$tmp_console"
  normalize_stream < "$OUTPUT" > "$tmp_file"
  if ! cmp -s "$tmp_console" "$tmp_file"; then
    echo "[fail] console vs file output differ — diff below:"
    diff -u "$tmp_console" "$tmp_file" || true
    rm -f "$tmp_console" "$tmp_file"
    exit 1
  fi
  rm -f "$tmp_console" "$tmp_file"
}

build() {
  if command -v cobc >/dev/null 2>&1; then
    print_header build "cobc found; compiling..."
    cobc -x "$SRC" -o "$BIN"
    print_header build "build succeeded"
  elif [[ -x "$BIN" ]]; then
    print_header build "cobc not found; using existing binary '$BIN'"
  else
    echo "[build] ERROR: cobc not found and '$BIN' is missing" >&2
    exit 1
  fi
}

# --- tests -----------------------------------------------------------------

run_success_first() {
  local user="$1" pass="$2"
  print_header test "success-first"
  printf '1\n%s\n%s\n' "$user" "$pass" > "$INPUT"
  : > "$OUTPUT"

  set +e; console_raw="$(INCOLLEGE_INTERACTIVE=0 $BIN 2>&1)"; status=$?; set -e

  # Show one transcript (console only)
  print_header console ""
  printf "%s\n" "$console_raw" | normalize_stream

  # Verify file mirror matches console
  check_mirror_matches "$console_raw"

  # Assertions
  local welcome; welcome=$(WELCOME_USER_MSG "$user")
  if printf "%s\n" "$console_raw" | normalize_stream | grep -Fq "Enter Your Choice: 1" \
     && printf "%s\n" "$console_raw" | normalize_stream | grep -Fq "$SUCCESS_MSG" \
     && printf "%s\n" "$console_raw" | normalize_stream | grep -Fq "$welcome" \
     && [[ $status -eq 0 ]]; then
    print_header pass "success-first"
  else
    print_header fail "success-first"; exit 1
  fi
}

run_failure_4_then_success() {
  local user_correct="$1" pass_correct="$2"
  print_header test "failure-x4-then-success"
  {
    printf '1\n'
    for i in 1 2 3 4; do printf 'wronguser%s\nbadpass%s\n' "$i" "$i"; done
    printf '%s\n%s\n' "$user_correct" "$pass_correct"
  } > "$INPUT"
  : > "$OUTPUT"

  set +e; console_raw="$(INCOLLEGE_INTERACTIVE=0 $BIN 2>&1)"; status=$?; set -e

  # Show one transcript (console only)
  print_header console ""
  printf "%s\n" "$console_raw" | normalize_stream

  # Verify file mirror matches console
  check_mirror_matches "$console_raw"

  # Assertions
  local welcome; welcome=$(WELCOME_USER_MSG "$user_correct")
  local fail_count
  fail_count=$(printf "%s\n" "$console_raw" | normalize_stream | grep -F "$FAIL_MSG" | wc -l | tr -d ' ')
  if printf "%s\n" "$console_raw" | normalize_stream | grep -Fq "Enter Your Choice: 1" \
     && [[ "$fail_count" -eq 4 ]] \
     && printf "%s\n" "$console_raw" | normalize_stream | grep -Fq "$SUCCESS_MSG" \
     && printf "%s\n" "$console_raw" | normalize_stream | grep -Fq "$welcome" \
     && [[ $status -eq 0 ]]; then
    print_header pass "failure-x4-then-success"
  else
    print_header fail "failure-x4-then-success (fails=$fail_count)"; exit 1
  fi
}

# --- main ------------------------------------------------------------------

main() {
  build

  # Ensure users file has the expected default user
  if ! grep -q '^testuser|Password1!$' "$USERS" 2>/dev/null; then
    print_header setup "adding default user to $USERS"
    mkdir -p "$(dirname "$USERS")"
    printf '%s\n' 'testuser|Password1!' >> "$USERS"
  fi

  mkdir -p io data

  mode=${1:-suite}
  case "$mode" in
    suite)
      run_success_first "testuser" "Password1!"
      run_failure_4_then_success "testuser" "Password1!"
      # uncomment if you want the pure-failure case in the suite:
      # run_failure_only_5
      ;;
    success)
      run_success_first "testuser" "Password1!";;
    failure-4-then-success)
      run_failure_4_then_success "testuser" "Password1!";;
    failure-5)
      run_failure_only_5;;
    *)
      echo "Usage: $0 [suite|success|failure-4-then-success|failure-5]"; exit 2;;
  esac

  print_header done "all tests passed"
}

main "$@"
