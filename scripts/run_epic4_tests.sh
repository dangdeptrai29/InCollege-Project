#!/usr/bin/env bash
set -euo pipefail

BIN=./InCollegeTest
ROOT="$(pwd)"
IO_IN=io/InCollege-Input.txt
IO_OUT=io/InCollege-Output.txt

# Reset baseline DB: users + profiles are fixed, just ensure present
setup_master_db() {
  cat > data/users.txt <<'EOF'
alice|alice123
bob|bob123
carol|carol123
TestUser|Password123!
EOF

  cat > data/profiles.txt <<'EOF'
alice|Alice|Smith|USF|Computer Science|2025|||
bob|Bob|Tran|UCF|Cybersecurity|2026|||
carol|Carol|Nguyen|UF|Business|2027|||
testuser|Test|User|—|—|—|||
newstudent|New|Student|West Coast Uni|Business|2027|||
EOF
}

# Configure connections/requests depending on case
setup_case_db() {
  local NAME="$1"
  :> data/connections.txt
  :> data/requests.txt

  case "$NAME" in
    TC01_view_none)
      # no connections/requests
      ;;
    TC02_send_request)
      # alice requests bob (starts empty, request will be created)
      ;;
    TC03_already_connected)
      echo "alice|bob|A" > data/connections.txt
      ;;
    TC04_reverse_pending)
      echo "bob|alice" > data/requests.txt
      ;;
    TC05_view_multiple)
      printf "bob|alice\ncarol|alice\n" > data/requests.txt
      ;;
    TC06_sample_week4)
      # no preconnections/requests
      ;;
  esac
}

run_case () {
  local NAME="$1"
  local IN="tests/epic4_inputs/${NAME}.txt"
  local EXP="tests/epic4_expected/${NAME}.out.txt"

  echo "=== Running ${NAME} ==="

  setup_case_db "$NAME"

  cp "${IN}" "${IO_IN}"
  :> "${IO_OUT}"

  ${BIN} >/dev/null

  if diff -u "${EXP}" "${IO_OUT}" > "io/${NAME}.diff.txt"; then
    echo -e "✅ [PASS] ${NAME}"
    rm -f "io/${NAME}.diff.txt"
  else
    echo -e "❌ [FAIL] ${NAME}  (see io/${NAME}.diff.txt)"
    sed -n '1,40p' "io/${NAME}.diff.txt"
  fi
  echo
}

# Ensure build
if [[ ! -x "${BIN}" ]]; then
  echo "Building InCollegeTest..."
  cobc -x -free -o InCollegeTest src/InCollege.cob
fi

# Always reset master DB once
setup_master_db

# Run all cases
run_case TC01_view_none
run_case TC02_send_request
run_case TC03_already_connected
run_case TC04_reverse_pending
run_case TC05_view_multiple
run_case TC06_sample_week4
