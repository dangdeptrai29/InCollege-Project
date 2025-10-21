#!/usr/bin/env bash
# This script runs Epic 6 tests for the InCollege program with a two-phase approach:
# Phase 1 (Setup): Populate initial data and create test scenario
#   - Special handling: Creates profiles directly in profiles.txt since menu option is unavailable
# Phase 2 (Test): Execute actual test case
# The database is refreshed between test cases to ensure isolation.

# --- Configuration ---
# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Directory paths
TEST_DIR="tests"
SETUP_DIR="${TEST_DIR}/epic6/setups"
INPUT_DIR="${TEST_DIR}/epic6/inputs"
EXPECTED_DIR="${TEST_DIR}/epic6/expected"
OUTPUT_DIR="${TEST_DIR}/epic6/outputs"
TEMP_DIR="temp"
DATA_DIR="data"
BACKUP_DIR="$TEMP_DIR/backup"

# File paths
SOURCE_FILE="src/InCollege.cob"
EXECUTABLE="./incollege"
INPUT_FILE="io/InCollege-Input.txt"
OUTPUT_FILE="io/InCollege-Output.txt"

# Test configuration
TEST_START=1
TEST_END=10
SEPARATOR="@@@@@@@@@@"

# --- Step 1: Force Compilation ---
echo "Compiling the program to ensure latest changes..."
cobc -x -o $EXECUTABLE $SOURCE_FILE
if [ $? -ne 0 ]; then
    echo -e "${RED}FAIL: Compilation failed.${NC}"
    exit 1
fi
echo -e "${GREEN}Compilation successful.${NC}"
echo "--------------------------------------------------"

# --- Step 2: Run Tests with Setup Phase ---

mkdir -p $TEMP_DIR
mkdir -p $BACKUP_DIR
mkdir -p $OUTPUT_DIR
mkdir -p $SETUP_DIR
mkdir -p $INPUT_DIR
mkdir -p $EXPECTED_DIR
mkdir -p $DATA_DIR

# Function to backup original data files
backup_data() {
    echo -e "${BLUE}Backing up original data files...${NC}"
    cp -f "$DATA_DIR/users.txt" "$BACKUP_DIR/users.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/profiles.txt" "$BACKUP_DIR/profiles.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/connections.txt" "$BACKUP_DIR/connections.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/requests.txt" "$BACKUP_DIR/requests.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/jobs.txt" "$BACKUP_DIR/jobs.txt.orig" 2>/dev/null || true
}

# Function to reset data files to clean/empty state
# For Epic 6 tests, we always start with empty database
reset_data() {
    echo -e "${YELLOW}Resetting database to clean state...${NC}"
    # Remove current data files
    rm -f "$DATA_DIR/users.txt"
    rm -f "$DATA_DIR/profiles.txt"
    rm -f "$DATA_DIR/connections.txt"
    rm -f "$DATA_DIR/requests.txt"
    rm -f "$DATA_DIR/jobs.txt"
    
    # Create empty files for Epic 6 tests (no backup restoration)
    touch "$DATA_DIR/users.txt"
    touch "$DATA_DIR/profiles.txt"
    touch "$DATA_DIR/connections.txt"
    touch "$DATA_DIR/requests.txt"
    touch "$DATA_DIR/jobs.txt"
}

# Function to create profile directly in profiles.txt
# Usage: create_profile_direct "username" "first" "last" "university" "major" "year" "about"
create_profile_direct() {
    local username="$1"
    local first="$2"
    local last="$3"
    local university="$4"
    local major="$5"
    local year="$6"
    local about="$7"
    
    echo -e "${BLUE}      Creating profile for ${username} directly in profiles.txt${NC}"
    
    # Append to profiles.txt in the correct format
    # Format: username|first|last|university|major|year|about|experiences|educations
    echo "${username}|${first}|${last}|${university}|${major}|${year}|${about}||" >> "$DATA_DIR/profiles.txt"
}

# Function to process setup input and extract profile creation data
# This function reads the setup input and creates profiles directly
process_setup_with_profiles() {
    local setup_file="$1"
    local test_case_num="$2"
    
    echo -e "${YELLOW}Phase 1: Running setup for Test Case ${test_case_num}...${NC}"
    
    # Reset database to clean state
    reset_data
    
    # Split setup file by separator
    setup_temp_dir="$TEMP_DIR/setup_${test_case_num}_parts"
    mkdir -p "$setup_temp_dir"
    
    # Split the setup file by separator
    awk -v sep="$SEPARATOR" -v dir="$setup_temp_dir" '
    BEGIN { part=0; file=dir"/part_"part".txt" }
    $0 == sep { close(file); part++; file=dir"/part_"part".txt"; next }
    { print > file }
    END { close(file) }
    ' "$setup_file"
    
    # Count how many parts we have
    part_count=$(ls -1 "$setup_temp_dir"/part_*.txt 2>/dev/null | wc -l)
    
    if [ $part_count -eq 0 ]; then
        echo -e "${RED}  WARNING: No setup parts found${NC}"
    else
        echo -e "${BLUE}  Setup has ${part_count} execution(s)${NC}"
        
        # Run each part sequentially
        for part_file in "$setup_temp_dir"/part_*.txt; do
            if [ -s "$part_file" ]; then
                part_num=$(basename "$part_file" | sed 's/part_//;s/.txt//')
                echo -e "${BLUE}    Running setup execution $((part_num + 1))...${NC}"
                
                # Check if this part is a profile creation (starts with "1" and contains profile data)
                first_line=$(head -n 1 "$part_file")
                line_count=$(wc -l < "$part_file")
                
                # Profile creation pattern: login (1) -> username -> password -> then 8 more lines of profile data
                if [[ "$first_line" == "1" ]] && [[ $line_count -ge 11 ]]; then
                    # This looks like a profile creation - extract data and create directly
                    username=$(sed -n '2p' "$part_file")
                    password=$(sed -n '3p' "$part_file")
                    choice=$(sed -n '4p' "$part_file")
                    
                    # Check if line 4 is "1" (indicating profile creation after login)
                    if [[ "$choice" == "1" ]]; then
                        first=$(sed -n '5p' "$part_file")
                        last=$(sed -n '6p' "$part_file")
                        university=$(sed -n '7p' "$part_file")
                        major=$(sed -n '8p' "$part_file")
                        year=$(sed -n '9p' "$part_file")
                        about=$(sed -n '10p' "$part_file")
                        
                        # Create profile directly
                        create_profile_direct "$username" "$first" "$last" "$university" "$major" "$year" "$about"
                        
                        # Create modified input that just logs in and exits (skip profile creation prompts)
                        echo "1" > "$setup_temp_dir/modified_part_${part_num}.txt"
                        echo "$username" >> "$setup_temp_dir/modified_part_${part_num}.txt"
                        echo "$password" >> "$setup_temp_dir/modified_part_${part_num}.txt"
                        echo "DONE" >> "$setup_temp_dir/modified_part_${part_num}.txt"
                        
                        part_file="$setup_temp_dir/modified_part_${part_num}.txt"
                    fi
                fi
                
                # Copy this part as input
                cp "$part_file" "$INPUT_FILE"
                
                # Run the program
                $EXECUTABLE > "$TEMP_DIR/setup_output_${test_case_num}_part_${part_num}.txt" 2>&1
                
                if [ $? -ne 0 ]; then
                    echo -e "${RED}    WARNING: Setup execution $((part_num + 1)) had non-zero exit code${NC}"
                fi
            fi
        done
    fi
    
    echo -e "${GREEN}  Setup complete. Database populated.${NC}"
    
    # Optional: Save database state after setup for inspection
    cp "$DATA_DIR/users.txt" "$TEMP_DIR/users_after_setup_${test_case_num}.txt" 2>/dev/null || true
    cp "$DATA_DIR/profiles.txt" "$TEMP_DIR/profiles_after_setup_${test_case_num}.txt" 2>/dev/null || true
    cp "$DATA_DIR/connections.txt" "$TEMP_DIR/connections_after_setup_${test_case_num}.txt" 2>/dev/null || true
    cp "$DATA_DIR/requests.txt" "$TEMP_DIR/requests_after_setup_${test_case_num}.txt" 2>/dev/null || true
    cp "$DATA_DIR/jobs.txt" "$TEMP_DIR/jobs_after_setup_${test_case_num}.txt" 2>/dev/null || true
    
    # Cleanup setup parts
    rm -rf "$setup_temp_dir"
}

# Backup original data before starting tests (for restoration after all tests complete)
backup_data

# For Epic 6, we start with clean database - clear any existing backup to force clean state
echo -e "${BLUE}Epic 6 tests will use clean/empty database (no existing users)${NC}"

# Loop through all test cases in numeric order
for i in $(seq $TEST_START $TEST_END); do
    setup_input="$SETUP_DIR/Setup_${i}.txt"
    test_input="$INPUT_DIR/Input_${i}.txt"
    
    # Skip if test input doesn't exist
    if [ ! -f "$test_input" ]; then
        continue
    fi
    
    target_output_file="$OUTPUT_DIR/Output_${i}.txt"
    
    echo ""
    echo "=========================================="
    echo -e "${BLUE}Processing Test Case ${i}${NC}"
    echo "=========================================="
    
    # --- Phase 1: Setup (if setup file exists) ---
    if [ -f "$setup_input" ]; then
        process_setup_with_profiles "$setup_input" "$i"
    else
        echo -e "${YELLOW}Phase 1: No setup file found (Setup_${i}.txt). Using current database state.${NC}"
        # Reset to clean state even if no setup file
        reset_data
    fi
    
    # --- Phase 2: Execute Test ---
    echo -e "${YELLOW}Phase 2: Running test case ${i}...${NC}"
    
    # Copy test input file
    cp "$test_input" "$INPUT_FILE"
    
    # Run the actual test and capture console output
    $EXECUTABLE > "$TEMP_DIR/actual_console_output.txt" 2>&1
    
    if [ $? -ne 0 ]; then
        echo -e "${RED}  WARNING: Test had non-zero exit code${NC}"
    fi
    
    # Copy the captured output to the final destination
    cp "$TEMP_DIR/actual_console_output.txt" "$target_output_file"
    
    echo -e "${GREEN}  Test complete -> ${target_output_file}${NC}"
    
    # Optional: Save final database state for inspection
    cp "$DATA_DIR/users.txt" "$TEMP_DIR/users_after_test_${i}.txt" 2>/dev/null || true
    cp "$DATA_DIR/profiles.txt" "$TEMP_DIR/profiles_after_test_${i}.txt" 2>/dev/null || true
    cp "$DATA_DIR/connections.txt" "$TEMP_DIR/connections_after_test_${i}.txt" 2>/dev/null || true
    cp "$DATA_DIR/requests.txt" "$TEMP_DIR/requests_after_test_${i}.txt" 2>/dev/null || true
    cp "$DATA_DIR/jobs.txt" "$TEMP_DIR/jobs_after_test_${i}.txt" 2>/dev/null || true
    
    # --- Step 2.5: Compare with Expected Output ---
    expected_output_file="$EXPECTED_DIR/Output_${i}.txt"
    if [ -f "$expected_output_file" ]; then
        echo -e "${YELLOW}  Comparing with expected output...${NC}"
        
        if diff -q "$target_output_file" "$expected_output_file" > /dev/null 2>&1; then
            echo -e "${GREEN}  ✓ PASS: Output matches expected${NC}"
        else
            echo -e "${RED}  ✗ FAIL: Output differs from expected${NC}"
            echo -e "${YELLOW}  Generating diff report...${NC}"
            
            # Save detailed diff to file
            diff -u "$expected_output_file" "$target_output_file" > "$TEMP_DIR/diff_${i}.txt" 2>&1
            
            # Show first 20 lines of diff
            echo -e "${BLUE}  First 20 lines of diff (- expected, + actual):${NC}"
            head -n 20 "$TEMP_DIR/diff_${i}.txt" | sed 's/^/    /'
            echo -e "${YELLOW}  Full diff saved to: ${TEMP_DIR}/diff_${i}.txt${NC}"
        fi
    else
        echo -e "${YELLOW}  No expected output file found: ${expected_output_file}${NC}"
        echo -e "${YELLOW}  Skipping comparison.${NC}"
    fi
done

# --- Step 3: Restore Original Data ---
echo ""
echo "--------------------------------------------------"
echo -e "${BLUE}Restoring original database state...${NC}"
# Restore from backup if exists
if [ -f "$BACKUP_DIR/users.txt.orig" ]; then
    cp "$BACKUP_DIR/users.txt.orig" "$DATA_DIR/users.txt"
fi
if [ -f "$BACKUP_DIR/profiles.txt.orig" ]; then
    cp "$BACKUP_DIR/profiles.txt.orig" "$DATA_DIR/profiles.txt"
fi
if [ -f "$BACKUP_DIR/connections.txt.orig" ]; then
    cp "$BACKUP_DIR/connections.txt.orig" "$DATA_DIR/connections.txt"
fi
if [ -f "$BACKUP_DIR/requests.txt.orig" ]; then
    cp "$BACKUP_DIR/requests.txt.orig" "$DATA_DIR/requests.txt"
fi
if [ -f "$BACKUP_DIR/jobs.txt.orig" ]; then
    cp "$BACKUP_DIR/jobs.txt.orig" "$DATA_DIR/jobs.txt"
fi
echo -e "${GREEN}Database restored.${NC}"

# --- Step 4: Generate Test Summary ---
echo ""
echo "=========================================="
echo -e "${BLUE}Test Summary${NC}"
echo "=========================================="

# Count pass/fail
pass_count=0
fail_count=0
skip_count=0

for i in $(seq $TEST_START $TEST_END); do
    test_input="$INPUT_DIR/Input_${i}.txt"
    expected_output_file="$EXPECTED_DIR/Output_${i}.txt"
    target_output_file="$OUTPUT_DIR/Output_${i}.txt"
    
    if [ ! -f "$test_input" ]; then
        continue
    fi
    
    if [ ! -f "$expected_output_file" ]; then
        skip_count=$((skip_count + 1))
        continue
    fi
    
    if [ -f "$target_output_file" ] && diff -q "$target_output_file" "$expected_output_file" > /dev/null 2>&1; then
        pass_count=$((pass_count + 1))
    else
        fail_count=$((fail_count + 1))
        echo -e "${RED}  Test ${i}: FAILED (see ${TEMP_DIR}/diff_${i}.txt)${NC}"
    fi
done

total_tests=$((pass_count + fail_count + skip_count))
echo ""
echo -e "${GREEN}Passed: ${pass_count}${NC}"
echo -e "${RED}Failed: ${fail_count}${NC}"
echo -e "${YELLOW}Skipped: ${skip_count}${NC}"
echo -e "Total: ${total_tests}"

# --- Step 5: Cleanup temporary files (optional) ---
# Uncomment the following lines to remove temporary files after test run
# rm -rf $TEMP_DIR
# rm -f $INPUT_FILE $OUTPUT_FILE

echo ""
echo "--------------------------------------------------"
echo -e "${GREEN}Test execution complete!${NC}"
echo -e "${YELLOW}Intermediate files saved in: ${TEMP_DIR}/${NC}"

if [ $fail_count -gt 0 ]; then
    echo -e "${RED}Some tests failed. Review diff files in ${TEMP_DIR}/ for details.${NC}"
    exit 1
else
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
fi
