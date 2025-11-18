#!/bin/bash

# This script runs the EPIC10 regression tests across all previous epics (epic1-epic9)
# to ensure old functionality remains intact after changes.

#!/usr/bin/env bash
# ==== Epic 10 Regression Testing ====
# Runs all test cases from epic1 through epic9 to verify no regressions

# --- Configuration ---
# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Directory paths
TEST_BASE_DIR="tests/epic10/regression"
TEMP_DIR="temp/epic10_regression"
DATA_DIR="data"
BACKUP_DIR="${TEMP_DIR}/backup"

# File paths
SOURCE_FILE="src/InCollege.cob"
EXECUTABLE="./incollege"
INPUT_FILE="io/InCollege-Input.txt"
OUTPUT_FILE="io/InCollege-Output.txt"

# Epic range to test
EPIC_START=1
EPIC_END=9

# --- Step 1: Force Compilation ---
echo "=========================================="
echo -e "${BLUE}Epic 10 Regression Testing${NC}"
echo "=========================================="
echo ""
echo "Compiling the program to ensure latest changes..."
cobc -free -x -o $EXECUTABLE $SOURCE_FILE
if [ $? -ne 0 ]; then
    echo -e "${RED}FAIL: Compilation failed.${NC}"
    exit 1
fi
echo -e "${GREEN}Compilation successful.${NC}"
echo "--------------------------------------------------"

# --- Step 2: Setup Directories ---
mkdir -p $TEMP_DIR
mkdir -p $BACKUP_DIR
mkdir -p $DATA_DIR

# Function to backup original data files
backup_data() {
    echo -e "${BLUE}Backing up original data files...${NC}"
    mkdir -p "$BACKUP_DIR"
    cp -f "$DATA_DIR/users.txt" "$BACKUP_DIR/users.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/profiles.txt" "$BACKUP_DIR/profiles.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/connections.txt" "$BACKUP_DIR/connections.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/jobs.txt" "$BACKUP_DIR/jobs.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/applications.txt" "$BACKUP_DIR/applications.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/messages.txt" "$BACKUP_DIR/messages.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/requests.txt" "$BACKUP_DIR/requests.txt.orig" 2>/dev/null || true
    cp -f "$DATA_DIR/search_history.txt" "$BACKUP_DIR/search_history.txt.orig" 2>/dev/null || true
    echo -e "${GREEN}Backup complete.${NC}"
}

# Function to reset data files to clean/empty state
reset_data() {
    echo -e "${YELLOW}  Resetting database to clean state...${NC}"
    # Remove all data files
    rm -f "$DATA_DIR/users.txt"
    rm -f "$DATA_DIR/profiles.txt"
    rm -f "$DATA_DIR/connections.txt"
    rm -f "$DATA_DIR/jobs.txt"
    rm -f "$DATA_DIR/applications.txt"
    rm -f "$DATA_DIR/messages.txt"
    rm -f "$DATA_DIR/requests.txt"
    rm -f "$DATA_DIR/search_history.txt"
    
    # Create empty files
    touch "$DATA_DIR/users.txt"
    touch "$DATA_DIR/profiles.txt"
    touch "$DATA_DIR/connections.txt"
    touch "$DATA_DIR/jobs.txt"
    touch "$DATA_DIR/applications.txt"
    touch "$DATA_DIR/messages.txt"
    touch "$DATA_DIR/requests.txt"
    touch "$DATA_DIR/search_history.txt"
}

# Function to load setup data files (Type 1: Direct data files)
# Loads users.txt, profiles.txt, connections.txt, etc. from setup directory
load_setup_data() {
    local setup_dir="$1"
    local test_name="$2"
    
    if [ ! -d "$setup_dir" ]; then
        return 1
    fi
    
    echo -e "${BLUE}  Loading setup data from: ${setup_dir}${NC}"
    
    # Check for test-specific subdirectory (Epic 8 style)
    if [ -d "${setup_dir}/${test_name}" ]; then
        local test_setup_dir="${setup_dir}/${test_name}"
        [ -f "${test_setup_dir}/users.txt" ] && cp "${test_setup_dir}/users.txt" "$DATA_DIR/users.txt" && echo -e "${GREEN}    ✓ Loaded users.txt${NC}"
        [ -f "${test_setup_dir}/profiles.txt" ] && cp "${test_setup_dir}/profiles.txt" "$DATA_DIR/profiles.txt" && echo -e "${GREEN}    ✓ Loaded profiles.txt${NC}"
        [ -f "${test_setup_dir}/connections.txt" ] && cp "${test_setup_dir}/connections.txt" "$DATA_DIR/connections.txt" && echo -e "${GREEN}    ✓ Loaded connections.txt${NC}"
        [ -f "${test_setup_dir}/jobs.txt" ] && cp "${test_setup_dir}/jobs.txt" "$DATA_DIR/jobs.txt" && echo -e "${GREEN}    ✓ Loaded jobs.txt${NC}"
        [ -f "${test_setup_dir}/applications.txt" ] && cp "${test_setup_dir}/applications.txt" "$DATA_DIR/applications.txt" && echo -e "${GREEN}    ✓ Loaded applications.txt${NC}"
        [ -f "${test_setup_dir}/messages.txt" ] && cp "${test_setup_dir}/messages.txt" "$DATA_DIR/messages.txt" && echo -e "${GREEN}    ✓ Loaded messages.txt${NC}"
        [ -f "${test_setup_dir}/requests.txt" ] && cp "${test_setup_dir}/requests.txt" "$DATA_DIR/requests.txt" && echo -e "${GREEN}    ✓ Loaded requests.txt${NC}"
        return 0
    fi
    
    # Check for named setup files (Epic 9 style: TC01_messages.txt)
    local base_name="${test_name%_input.txt}"
    base_name="${base_name%_output.txt}"
    
    if [ -f "${setup_dir}/${base_name}_messages.txt" ]; then
        cp "${setup_dir}/${base_name}_messages.txt" "$DATA_DIR/messages.txt" && echo -e "${GREEN}    ✓ Loaded ${base_name}_messages.txt${NC}"
    fi
    
    # Load common setup files (Epic 1-5, 7 style)
    [ -f "${setup_dir}/users.txt" ] && cp "${setup_dir}/users.txt" "$DATA_DIR/users.txt" && echo -e "${GREEN}    ✓ Loaded users.txt${NC}"
    [ -f "${setup_dir}/profiles.txt" ] && cp "${setup_dir}/profiles.txt" "$DATA_DIR/profiles.txt" && echo -e "${GREEN}    ✓ Loaded profiles.txt${NC}"
    [ -f "${setup_dir}/connections.txt" ] && cp "${setup_dir}/connections.txt" "$DATA_DIR/connections.txt" && echo -e "${GREEN}    ✓ Loaded connections.txt${NC}"
    [ -f "${setup_dir}/jobs.txt" ] && cp "${setup_dir}/jobs.txt" "$DATA_DIR/jobs.txt" && echo -e "${GREEN}    ✓ Loaded jobs.txt${NC}"
    [ -f "${setup_dir}/applications.txt" ] && cp "${setup_dir}/applications.txt" "$DATA_DIR/applications.txt" && echo -e "${GREEN}    ✓ Loaded applications.txt${NC}"
    [ -f "${setup_dir}/messages.txt" ] && cp "${setup_dir}/messages.txt" "$DATA_DIR/messages.txt" && echo -e "${GREEN}    ✓ Loaded messages.txt${NC}"
    [ -f "${setup_dir}/requests.txt" ] && cp "${setup_dir}/requests.txt" "$DATA_DIR/requests.txt" && echo -e "${GREEN}    ✓ Loaded requests.txt${NC}"
    
    # Epic 7 style: test-specific named setup files
    # Map test names to appropriate setup files
    if [ -f "${setup_dir}/users_test.txt" ]; then
        cp "${setup_dir}/users_test.txt" "$DATA_DIR/users.txt"
        echo -e "${GREEN}    ✓ Loaded users_test.txt${NC}"
    fi
    
    # Determine which job file to load based on test name
    if [[ "$test_name" == *"empty_browse"* ]]; then
        [ -f "${setup_dir}/jobs_empty.txt" ] && cp "${setup_dir}/jobs_empty.txt" "$DATA_DIR/jobs.txt" && echo -e "${GREEN}    ✓ Loaded jobs_empty.txt${NC}"
        [ -f "${setup_dir}/applications_empty.txt" ] && cp "${setup_dir}/applications_empty.txt" "$DATA_DIR/applications.txt" && echo -e "${GREEN}    ✓ Loaded applications_empty.txt${NC}"
    elif [[ "$test_name" == *"single_job"* ]]; then
        [ -f "${setup_dir}/jobs_single.txt" ] && cp "${setup_dir}/jobs_single.txt" "$DATA_DIR/jobs.txt" && echo -e "${GREEN}    ✓ Loaded jobs_single.txt${NC}"
        [ -f "${setup_dir}/applications_empty.txt" ] && cp "${setup_dir}/applications_empty.txt" "$DATA_DIR/applications.txt" && echo -e "${GREEN}    ✓ Loaded applications_empty.txt${NC}"
    elif [[ "$test_name" == *"multiple_jobs"* ]] || [[ "$test_name" == *"view_details"* ]] || [[ "$test_name" == *"invalid_job"* ]]; then
        [ -f "${setup_dir}/jobs_multiple.txt" ] && cp "${setup_dir}/jobs_multiple.txt" "$DATA_DIR/jobs.txt" && echo -e "${GREEN}    ✓ Loaded jobs_multiple.txt${NC}"
        [ -f "${setup_dir}/applications_empty.txt" ] && cp "${setup_dir}/applications_empty.txt" "$DATA_DIR/applications.txt" && echo -e "${GREEN}    ✓ Loaded applications_empty.txt${NC}"
    fi
    
    return 0
}

# Function to run interactive setup script (Type 2: Epic 6 style with @@@@ separator)
run_setup_script() {
    local setup_file="$1"
    local test_num="$2"
    
    if [ ! -f "$setup_file" ]; then
        return 1
    fi
    
    echo -e "${BLUE}  Running setup script: $(basename $setup_file)${NC}"
    
    # Split setup file by separator
    local setup_temp_dir="$TEMP_DIR/setup_${test_num}_parts"
    mkdir -p "$setup_temp_dir"
    
    awk -v sep="@@@@@@@@@@" -v dir="$setup_temp_dir" '
    BEGIN { part=0; file=dir"/part_"part".txt" }
    $0 == sep { close(file); part++; file=dir"/part_"part".txt"; next }
    { print > file }
    END { close(file) }
    ' "$setup_file"
    
    # Run each part sequentially
    for part_file in "$setup_temp_dir"/part_*.txt; do
        if [ -s "$part_file" ]; then
            part_num=$(basename "$part_file" | sed 's/part_//;s/.txt//')
            echo -e "${BLUE}    Running setup part $((part_num + 1))...${NC}"
            
            cp "$part_file" "$INPUT_FILE"
            $EXECUTABLE > "$TEMP_DIR/setup_output_${test_num}_part_${part_num}.txt" 2>&1
            
            if [ $? -ne 0 ]; then
                echo -e "${YELLOW}    WARNING: Setup part $((part_num + 1)) had non-zero exit code${NC}"
            fi
        fi
    done
    
    rm -rf "$setup_temp_dir"
    return 0
}

# Backup original data before starting tests
backup_data

# Initialize test counters
total_tests=0
pass_count=0
fail_count=0
skip_count=0

# Loop through each epic
for epic_num in $(seq $EPIC_START $EPIC_END); do
    epic_dir="${TEST_BASE_DIR}/epic${epic_num}"
    
    # Check if epic directory exists
    if [ ! -d "$epic_dir" ]; then
        echo -e "${YELLOW}Skipping epic${epic_num} - directory not found${NC}"
        continue
    fi
    
    input_dir="${epic_dir}/inputs"
    expected_dir="${epic_dir}/expected"
    output_dir="${epic_dir}/outputs"
    
    # Create output directory for this epic
    mkdir -p "$output_dir"
    
    # Check if input directory exists
    if [ ! -d "$input_dir" ]; then
        echo -e "${YELLOW}Skipping epic${epic_num} - no inputs directory${NC}"
        continue
    fi
    
    echo ""
    echo "=========================================="
    echo -e "${BLUE}Testing Epic ${epic_num}${NC}"
    echo "=========================================="
    
    # Get all input files and sort them
    input_files=$(find "$input_dir" -type f -name "*.txt" | sort)
    
    if [ -z "$input_files" ]; then
        echo -e "${YELLOW}No test files found in epic${epic_num}/inputs${NC}"
        continue
    fi
    
    # Process each input file
    for input_file in $input_files; do
        # Extract filename without path
        input_filename=$(basename "$input_file")
        
        # Determine expected output filename (handle different naming conventions)
        # Pattern: Input_X.txt -> Output_X.txt OR InCollege-Input.txt -> InCollege-Output.txt
        # OR TC01_input.txt -> TC01_output.txt
        expected_filename=""
        if [[ "$input_filename" =~ ^Input_(.+)\.txt$ ]]; then
            expected_filename="Output_${BASH_REMATCH[1]}.txt"
        elif [[ "$input_filename" =~ ^InCollege-Input\.txt$ ]]; then
            expected_filename="InCollege-Output.txt"
        elif [[ "$input_filename" =~ ^(.+)_input\.txt$ ]]; then
            expected_filename="${BASH_REMATCH[1]}_output.txt"
        elif [[ "$input_filename" =~ ^(.+)input\.txt$ ]]; then
            expected_filename="${BASH_REMATCH[1]}output.txt"
        else
            # Try to find matching output file by replacing "input" with "output"
            expected_filename="${input_filename//input/output}"
            expected_filename="${expected_filename//Input/Output}"
        fi
        
        expected_file="${expected_dir}/${expected_filename}"
        output_file="${output_dir}/${expected_filename}"
        
        total_tests=$((total_tests + 1))
        
        echo ""
        echo "------------------------------------------"
        echo -e "${BLUE}Test: ${input_filename}${NC}"
        echo "------------------------------------------"
        
        # Reset database before each test
        reset_data
        
        # Setup data loading strategy
        setup_dir="${epic_dir}/setups"
        test_base_name="${input_filename%.txt}"
        test_base_name="${test_base_name%_input}"
        setup_loaded=false
        
        # Try Type 2 FIRST: Interactive setup script (Epic 6 style with @@@@)
        if [[ "$input_filename" =~ ^Input_([0-9]+)\.txt$ ]]; then
            setup_script="${setup_dir}/Setup_${BASH_REMATCH[1]}.txt"
            if run_setup_script "$setup_script" "${epic_num}_${BASH_REMATCH[1]}"; then
                echo -e "${GREEN}  Setup script executed${NC}"
                setup_loaded=true
            fi
        fi
        
        # If no setup script, try Type 1: Data files
        if [ "$setup_loaded" = false ]; then
            if load_setup_data "$setup_dir" "$test_base_name"; then
                echo -e "${GREEN}  Setup data loaded${NC}"
                setup_loaded=true
            fi
        fi
        
        # If still no setup found, warn user
        if [ "$setup_loaded" = false ]; then
            echo -e "${YELLOW}  No setup data found - using clean database${NC}"
        fi
        
        # Copy input file to program input location
        cp "$input_file" "$INPUT_FILE"
        
        # Run the program and capture output
        $EXECUTABLE > "$output_file" 2>&1
        exit_code=$?
        
        if [ $exit_code -ne 0 ]; then
            echo -e "${YELLOW}  WARNING: Program exited with code ${exit_code}${NC}"
        fi
        
        echo -e "${GREEN}  Output saved to: ${output_file}${NC}"
        
        # Compare with expected output if it exists
        if [ -f "$expected_file" ]; then
            echo -e "${YELLOW}  Comparing with expected output...${NC}"
            
            if diff -q "$output_file" "$expected_file" > /dev/null 2>&1; then
                echo -e "${GREEN}  ✓ PASS: Output matches expected${NC}"
                pass_count=$((pass_count + 1))
            else
                echo -e "${RED}  ✗ FAIL: Output differs from expected${NC}"
                fail_count=$((fail_count + 1))
                
                # Save diff to file
                diff_file="${TEMP_DIR}/diff_epic${epic_num}_${input_filename}.txt"
                diff -u "$expected_file" "$output_file" > "$diff_file" 2>&1
                
                # Show first 20 lines of diff
                echo -e "${BLUE}  First 20 lines of diff (- expected, + actual):${NC}"
                head -n 20 "$diff_file" | sed 's/^/    /'
                echo -e "${YELLOW}  Full diff: ${diff_file}${NC}"
            fi
        else
            echo -e "${YELLOW}  ⚠ SKIP: No expected output file found${NC}"
            echo -e "${YELLOW}  Expected: ${expected_file}${NC}"
            skip_count=$((skip_count + 1))
        fi
    done
done

# --- Step 3: Restore Original Data ---
echo ""
echo "=========================================="
echo -e "${BLUE}Restoring original database state...${NC}"
echo "=========================================="

# Restore from backup if exists
[ -f "$BACKUP_DIR/users.txt.orig" ] && cp "$BACKUP_DIR/users.txt.orig" "$DATA_DIR/users.txt"
[ -f "$BACKUP_DIR/profiles.txt.orig" ] && cp "$BACKUP_DIR/profiles.txt.orig" "$DATA_DIR/profiles.txt"
[ -f "$BACKUP_DIR/connections.txt.orig" ] && cp "$BACKUP_DIR/connections.txt.orig" "$DATA_DIR/connections.txt"
[ -f "$BACKUP_DIR/jobs.txt.orig" ] && cp "$BACKUP_DIR/jobs.txt.orig" "$DATA_DIR/jobs.txt"
[ -f "$BACKUP_DIR/applications.txt.orig" ] && cp "$BACKUP_DIR/applications.txt.orig" "$DATA_DIR/applications.txt"
[ -f "$BACKUP_DIR/messages.txt.orig" ] && cp "$BACKUP_DIR/messages.txt.orig" "$DATA_DIR/messages.txt"
[ -f "$BACKUP_DIR/requests.txt.orig" ] && cp "$BACKUP_DIR/requests.txt.orig" "$DATA_DIR/requests.txt"
[ -f "$BACKUP_DIR/search_history.txt.orig" ] && cp "$BACKUP_DIR/search_history.txt.orig" "$DATA_DIR/search_history.txt"

echo -e "${GREEN}Database restored.${NC}"

# --- Step 4: Generate Test Summary ---
echo ""
echo "=========================================="
echo -e "${BLUE}REGRESSION TEST SUMMARY${NC}"
echo "=========================================="
echo ""
echo -e "Total Tests:   ${total_tests}"
echo -e "${GREEN}Passed:        ${pass_count}${NC}"
echo -e "${RED}Failed:        ${fail_count}${NC}"
echo -e "${YELLOW}Skipped:       ${skip_count}${NC}"
echo ""

# Calculate pass rate
if [ $total_tests -gt 0 ]; then
    pass_rate=$(awk "BEGIN {printf \"%.1f\", ($pass_count / $total_tests) * 100}")
    echo -e "Pass Rate:     ${pass_rate}%"
fi

echo "=========================================="
echo ""

# List failed tests if any
if [ $fail_count -gt 0 ]; then
    echo -e "${RED}Failed Tests:${NC}"
    # Find all diff files
    find "$TEMP_DIR" -name "diff_epic*.txt" | sort | while read diff_file; do
        echo -e "${RED}  - $(basename "$diff_file")${NC}"
    done
    echo ""
fi

echo -e "${YELLOW}Test outputs saved in: tests/epic10/regression/epic*/outputs/${NC}"
echo -e "${YELLOW}Diff files saved in: ${TEMP_DIR}/${NC}"
echo ""

# --- Step 5: Cleanup (optional) ---
# Uncomment to remove temporary files
# rm -rf $TEMP_DIR
# rm -f $INPUT_FILE $OUTPUT_FILE

echo "--------------------------------------------------"
if [ $fail_count -gt 0 ]; then
    echo -e "${RED}❌ REGRESSION TESTS FAILED${NC}"
    echo -e "${RED}Review diff files in ${TEMP_DIR}/ for details.${NC}"
    exit 1
else
    if [ $skip_count -gt 0 ]; then
        echo -e "${YELLOW}⚠️  ALL TESTS PASSED (with ${skip_count} skipped)${NC}"
    else
        echo -e "${GREEN}✅ ALL REGRESSION TESTS PASSED!${NC}"
    fi
    exit 0
fi
