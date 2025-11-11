#!/bin/bash

# Epic 9 - View Messages Positive Test Suite
# Tests all 10 positive scenarios for viewing messages

COBOL_SRC="src/InCollege.cob"
COBOL_BIN="InCollege"
TEST_DIR="tests/epic9"
INPUT_DIR="$TEST_DIR/inputs"
EXPECTED_DIR="$TEST_DIR/expected"
SETUP_DIR="$TEST_DIR/setups"
OUTPUT_DIR="$TEST_DIR/outputs"
DATA_DIR="data"
IO_DIR="io"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Compile COBOL program
echo "========================================="
echo "Compiling COBOL Program..."
echo "========================================="
cobc -x -free -o $COBOL_BIN $COBOL_SRC
if [ $? -ne 0 ]; then
    echo -e "${RED}Compilation failed!${NC}"
    exit 1
fi
echo -e "${GREEN}Compilation successful!${NC}\n"

# Setup test data files
setup_test_data() {
    local tc_num=$1
    mkdir -p $DATA_DIR
    
    # Check if test has custom setup
    if [ -f "$SETUP_DIR/TC${tc_num}_messages.txt" ]; then
        cp $SETUP_DIR/TC${tc_num}_messages.txt $DATA_DIR/messages.txt
    else
        # Default messages
        cat > $DATA_DIR/messages.txt << 'EOF'
sender1|receiver|Hi there! Glad we connected on InCollege.|20250728103000
sender2|receiver|Check out this interesting job posting I found!|20250729091500
sender1|receiver|Are you available for a quick call today?|20250730140000
EOF
    fi
    
    # Create users (sender1, sender2, receiver)
    cat > $DATA_DIR/users.txt << 'EOF'
sender1|Pass123!
sender2|Pass456!
receiver|Pass789!
EOF

    # Create profiles
    cat > $DATA_DIR/profiles.txt << 'EOF'
sender1|Alice|Smith|USF|CS|2024|Software Dev||
sender2|Bob|Jones|USF|IT|2024|Data Analyst||
receiver|Carol|White|USF|EE|2025|Student||
EOF

    # Create connections (all connected)
    cat > $DATA_DIR/connections.txt << 'EOF'
sender1|receiver|A
sender2|receiver|A
EOF

    # Create empty files for other data
    touch $DATA_DIR/jobs.txt
    touch $DATA_DIR/applications.txt
}

# Run single test case
run_test() {
    local tc_num=$1
    local tc_name=$2
    
    echo "========================================="
    echo "TC$tc_num: $tc_name"
    echo "========================================="
    
    # Check if input file exists
    if [ ! -f "$INPUT_DIR/TC${tc_num}_input.txt" ]; then
        echo -e "${RED}✗ SKIP - Input file missing${NC}"
        return 1
    fi
    
    # Setup fresh data
    setup_test_data $tc_num
    
    # Copy test input
    cp $INPUT_DIR/TC${tc_num}_input.txt $IO_DIR/InCollege-Input.txt
    
    # Run program
    mkdir -p $OUTPUT_DIR
    ./$COBOL_BIN > $OUTPUT_DIR/TC${tc_num}_output.txt 2>&1
    
    # Compare output
    if diff -w $OUTPUT_DIR/TC${tc_num}_output.txt $EXPECTED_DIR/TC${tc_num}_expected.txt > /dev/null 2>&1; then
        echo -e "${GREEN}✓ PASS${NC}"
        return 0
    else
        echo -e "${RED}✗ FAIL${NC}"
        echo "Differences:"
        diff -u $EXPECTED_DIR/TC${tc_num}_expected.txt $OUTPUT_DIR/TC${tc_num}_output.txt | head -20
        return 1
    fi
}

# Main test execution
mkdir -p $IO_DIR
mkdir -p $INPUT_DIR
mkdir -p $EXPECTED_DIR
mkdir -p $SETUP_DIR
mkdir -p $OUTPUT_DIR

passed=0
failed=0

# Run all 10 test cases
run_test "01" "View single message from one sender" && ((passed++)) || ((failed++))
run_test "02" "View multiple messages from same sender" && ((passed++)) || ((failed++))
run_test "03" "View messages from multiple senders" && ((passed++)) || ((failed++))
run_test "04" "View messages in chronological order" && ((passed++)) || ((failed++))
run_test "05" "View messages with timestamps" && ((passed++)) || ((failed++))
run_test "06" "View messages after persistence" && ((passed++)) || ((failed++))
run_test "07" "View messages with long content" && ((passed++)) || ((failed++))
run_test "08" "View messages with special characters" && ((passed++)) || ((failed++))
run_test "09" "View mixed old and new messages" && ((passed++)) || ((failed++))
run_test "10" "View messages multiple times" && ((passed++)) || ((failed++))

# Summary
echo ""
echo "========================================="
echo "TEST SUMMARY"
echo "========================================="
echo -e "Total: 10 | ${GREEN}Passed: $passed${NC} | ${RED}Failed: $failed${NC}"
echo "========================================="

exit $failed