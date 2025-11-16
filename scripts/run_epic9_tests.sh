#!/bin/bash

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
NC='\033[0m'

echo "========================================="
echo "Compiling COBOL Program..."
echo "========================================="
cobc -x -free -o $COBOL_BIN $COBOL_SRC
if [ $? -ne 0 ]; then
    echo -e "${RED}Compilation failed!${NC}"
    exit 1
fi
echo -e "${GREEN}Compilation successful!${NC}\n"

setup_test_data() {
    local tc_num=$1
    mkdir -p $DATA_DIR
    
    if [ -f "$SETUP_DIR/TC${tc_num}_messages.txt" ]; then
        cp $SETUP_DIR/TC${tc_num}_messages.txt $DATA_DIR/messages.txt
    else
        cat > $DATA_DIR/messages.txt << 'EOF2'
sender1|receiver|Hi there! Glad we connected on InCollege.|20250728103000
sender2|receiver|Check out this interesting job posting I found!|20250729091500
sender1|receiver|Are you available for a quick call today?|20250730140000
EOF2
    fi
    
    cat > $DATA_DIR/users.txt << 'EOF2'
sender1|Pass123!
sender2|Pass456!
receiver|Pass789!
EOF2

    cat > $DATA_DIR/profiles.txt << 'EOF2'
sender1|Alice|Smith|USF|CS|2024|Software Dev||
sender2|Bob|Jones|USF|IT|2024|Data Analyst||
receiver|Carol|White|USF|EE|2025|Student||
EOF2

    cat > $DATA_DIR/connections.txt << 'EOF2'
sender1|receiver|A
sender2|receiver|A
EOF2

    touch $DATA_DIR/jobs.txt
    touch $DATA_DIR/applications.txt
}

run_test() {
    local tc_num=$1
    local tc_name=$2
    
    echo "========================================="
    echo "TC$tc_num: $tc_name"
    echo "========================================="
    
    if [ ! -f "$INPUT_DIR/TC${tc_num}_input.txt" ]; then
        echo -e "${RED}✗ SKIP${NC}"
        return 1
    fi
    
    setup_test_data $tc_num
    cp $INPUT_DIR/TC${tc_num}_input.txt $IO_DIR/InCollege-Input.txt
    
    mkdir -p $OUTPUT_DIR
    ./$COBOL_BIN > $OUTPUT_DIR/TC${tc_num}_output.txt 2>&1
    
    if diff -w $OUTPUT_DIR/TC${tc_num}_output.txt $EXPECTED_DIR/TC${tc_num}_expected.txt > /dev/null 2>&1; then
        echo -e "${GREEN}✓ PASS${NC}"
        return 0
    else
        echo -e "${RED}✗ FAIL${NC}"
        diff -u $EXPECTED_DIR/TC${tc_num}_expected.txt $OUTPUT_DIR/TC${tc_num}_output.txt | head -20
        return 1
    fi
}

mkdir -p $IO_DIR $INPUT_DIR $EXPECTED_DIR $SETUP_DIR $OUTPUT_DIR

passed=0
failed=0

run_test "01" "View single message from one sender"
if [ $? -eq 0 ]; then ((passed++)); else ((failed++)); fi

run_test "02" "View multiple messages from same sender"
if [ $? -eq 0 ]; then ((passed++)); else ((failed++)); fi

run_test "03" "View messages from multiple senders"
if [ $? -eq 0 ]; then ((passed++)); else ((failed++)); fi

run_test "04" "View messages in chronological order"
if [ $? -eq 0 ]; then ((passed++)); else ((failed++)); fi

run_test "05" "View messages with timestamps"
if [ $? -eq 0 ]; then ((passed++)); else ((failed++)); fi

run_test "06" "View messages after persistence"
if [ $? -eq 0 ]; then ((passed++)); else ((failed++)); fi

run_test "07" "View messages with special characters"
if [ $? -eq 0 ]; then ((passed++)); else ((failed++)); fi

run_test "08" "View mixed old and new messages"
if [ $? -eq 0 ]; then ((passed++)); else ((failed++)); fi

run_test "09" "View messages multiple times"
if [ $? -eq 0 ]; then ((passed++)); else ((failed++)); fi

echo ""
echo "========================================="
echo "TEST SUMMARY"
echo "========================================="
echo -e "Total: 9 | ${GREEN}Passed: $passed${NC} | ${RED}Failed: $failed${NC}"
echo "========================================="

exit $failed
