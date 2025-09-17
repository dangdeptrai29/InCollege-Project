#!/usr/bin/env bash
# This script runs the InCollege program for each Input file and
# generates a corresponding Output file based on the program's console output.
# WARNING: Always manually verify the generated output files for correctness.

# --- Configuration ---
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

# Move to the project's root directory
# cd ..

# --- Step 1: Force Compilation ---
echo "Compiling the program to ensure latest changes..."
cobc -x -o ./incollege src/InCollege.cob
if [ $? -ne 0 ]; then
    echo -e "${RED}FAIL: Compilation failed.${NC}"
    exit 1
fi
echo "Compilation successful."
echo "--------------------------------------------------"

# --- Step 2: Generate Output for All Test Cases ---
TEST_DIR="tests"
TEMP_DIR="temp"
mkdir -p $TEMP_DIR

# Loop through all input files
for test_input in $TEST_DIR/Input_*.txt; do
    # Derive the target output filename by replacing "Input" with "Output"
    base_filename=$(basename "$test_input")
    target_output_file="$TEST_DIR/${base_filename/Input/Output}"

    echo -e "Running for: ${base_filename}"

    # Prepare for the run by copying the correct input file
    cp "$test_input" "io/InCollege-Input.txt"

    # Run the program and capture its console output to a temporary file
    ./incollege > "$TEMP_DIR/actual_console_output.txt" 2>&1

    # Copy the captured output to the final destination
    cp "$TEMP_DIR/actual_console_output.txt" "$target_output_file"

    echo -e "  - ${GREEN}Generated -> ${target_output_file}${NC}"
    echo ""
done

# --- Step 3: Cleanup ---
rm -rf $TEMP_DIR
# rm io/InCollege-Input.txt io/InCollege-Output.txt # Clean up runtime files

echo "--------------------------------------------------"
echo "Output generation complete."
echo -e "${RED}Next Step: Manually review each generated Output_XX.txt file to ensure it is correct.${NC}"

exit 0