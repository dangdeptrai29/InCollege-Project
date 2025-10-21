#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

mkdir -p tests/epic5_outputs io data

echo "🏗️  Building COBOL..."
cobc -x -free -o InCollegeTest src/InCollege.cob

seed_base_data() {
  echo "🧩 Seeding base users and profiles..."
  cat > data/users.txt <<'EOF'
alice|alice123
bob|bob123
testuser|Password123!
otheruser|testpass
friendb|fbpass
EOF

  cat > data/profiles.txt <<'EOF'
alice|Alice|Smith|USF|Computer Science|2025|||
bob|Bob|Tran|UCF|Cybersecurity|2026|||
testuser|Test|User|UF|Business|2027|||
otheruser|Other|User|Another U|Marketing|2026|||
friendb|Friend|B|Big State|Engineering|2024|||
EOF
}

run_case() {
  local name="$1"
  echo "🚀 Running $name ..."

  # Reset Week-5 persistence
  :> data/connections.txt
  :> data/requests.txt   # not used by menu, but keep clean

  # Per-test seeding (use connections.txt with P/A)
  case "$name" in
    TC01_accept_single)
      # otheruser -> testuser (pending), we will accept
      echo "otheruser|testuser|P" > data/connections.txt
      ;;
    TC02_reject_single)
      # otheruser -> testuser (pending), we will reject
      echo "otheruser|testuser|P" > data/connections.txt
      ;;
    TC03_mixed_accept_reject)
      # two pendings to testuser: accept first, reject second (per input file)
      cat > data/connections.txt <<'CONN'
otheruser|testuser|P
friendb|testuser|P
CONN
      ;;
    TC04_view_network_after_accept)
      # already accepted connections, just list them
      cat > data/connections.txt <<'CONN'
otheruser|testuser|A
friendb|testuser|A
CONN
      ;;
    TC05_sample_week5)
      # assignment sample: one pending that we'll accept, plus an existing friend
      cat > data/connections.txt <<'CONN'
otheruser|testuser|P
friendb|testuser|A
CONN
      ;;
  esac

  # Wire input and run
  cp "tests/epic5_inputs/${name}.txt" io/InCollege-Input.txt
  :> io/InCollege-Output.txt
  ./InCollegeTest > "tests/epic5_outputs/${name}.out.txt" 2>&1 || true
  echo "✅ Output -> tests/epic5_outputs/${name}.out.txt"
}

seed_base_data

cases=(
  TC01_accept_single
  TC02_reject_single
  TC03_mixed_accept_reject
  TC04_view_network_after_accept
  TC05_sample_week5
)

for c in "${cases[@]}"; do
  if [[ -f "tests/epic5_inputs/${c}.txt" ]]; then
    run_case "$c"
  else
    echo "⚠️  Missing input: tests/epic5_inputs/${c}.txt"
  fi
done

echo "🎉 All Week 5 outputs generated successfully!"
