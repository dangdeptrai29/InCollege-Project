#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <test-id>"
  exit 1
fi

test_id="$1"
project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
inputs_dir="$project_root/tests/epic8/inputs"
setups_dir="$project_root/tests/epic8/setups"
outputs_dir="$project_root/tests/epic8/outputs"

input_file="$inputs_dir/${test_id}.txt"
setup_case_dir="$setups_dir/${test_id}"

if [[ ! -f "$project_root/InCollege" ]]; then
  echo "Error: executable 'InCollege' not found at project root."
  exit 1
fi

if [[ ! -f "$input_file" ]]; then
  echo "Error: missing input file $input_file"
  exit 1
fi

mkdir -p "$project_root/data" "$project_root/io" "$outputs_dir"
: > "$project_root/io/InCollege-Output.txt"
: > "$project_root/data/users.txt"
: > "$project_root/data/connections.txt"
: > "$project_root/data/messages.txt"
: > "$project_root/data/jobs.txt"
: > "$project_root/data/applications.txt"

if [[ -d "$setup_case_dir" ]]; then
  while IFS= read -r -d '' setup_file; do
    dest="$project_root/data/$(basename "$setup_file")"
    cp "$setup_file" "$dest"
  done < <(find "$setup_case_dir" -type f -print0)
fi

cp "$input_file" "$project_root/io/InCollege-Input.txt"

console_capture="$outputs_dir/${test_id}_console.txt"
./InCollege | tee "$console_capture"

cp "$project_root/io/InCollege-Output.txt" "$outputs_dir/${test_id}_log.txt"
cp "$project_root/data/messages.txt" "$outputs_dir/${test_id}_messages.txt"
