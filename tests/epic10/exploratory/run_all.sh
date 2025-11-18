#!/bin/bash
for i in {01..15}; do
    tc="TC$i"
    echo "=== Running $tc ==="
    
    rm -rf data/* io/*
    mkdir -p data io
    
    if [ -d "tests/epic10/exploratory/setups/$tc" ]; then
        cp tests/epic10/exploratory/setups/$tc/* data/ 2>/dev/null
    fi
    
    input=$(ls tests/epic10/exploratory/inputs/${tc}_*.txt 2>/dev/null | head -1)
    if [ -f "$input" ]; then
        cp "$input" io/InCollege-Input.txt
        ./InCollege > /dev/null 2>&1
        output_name=$(basename "$input" .txt)
        cp io/InCollege-Output.txt "tests/epic10/exploratory/outputs/${output_name}.txt"
        echo "✓ Output: tests/epic10/exploratory/outputs/${output_name}.txt"
    fi
done
