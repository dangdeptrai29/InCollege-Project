#!/usr/bin/env bash

# This file compare terminal output of the InCollege program with it self-documented file output AND provided sample output.

# To compare against provided sample output, modify io/InCollege_Input.txt and io/InCollege_SampleOutput.txt to test different scenarios.

cd ..
# Force compile the program
if [ -f "./incollege" ]; then
    rm incollege
fi
cd src
echo "Force compiling the program to ensure latest changes..."
cobc -x InCollege.cob -o ../incollege
cd ..


# Run the program and redirect output to a temporary file
mkdir -p temp
./incollege > temp/output.txt 2>&1

# Compare the output with the expected output
if diff -q temp/output.txt io/InCollege-Output.txt > /dev/null; then
    echo "- Terminal output matches file output."
else
    echo "- Terminal output does not match file output."
    echo "Differences:"
    diff temp/output.txt io/InCollege-Output.txt

    exit 1
fi

echo "---------"

#! Further testing
# Compare against Sample Output (Only use when doing E2E testing with expected output)

if diff -q temp/output.txt io/InCollege_SampleOutput.txt > /dev/null; then
    echo "- Terminal output matches the sample output."
else
    echo "- Terminal output does not match the sample output."
    echo "Differences:"
    diff temp/output.txt io/InCollege_SampleOutput.txt
fi

cd scripts