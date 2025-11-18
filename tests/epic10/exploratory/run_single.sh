#!/bin/bash
tc="TC$1"
rm -rf data/* io/*
mkdir -p data io
[ -d "tests/epic10/exploratory/setups/$tc" ] && cp tests/epic10/exploratory/setups/$tc/* data/
input=$(ls tests/epic10/exploratory/inputs/${tc}_*.txt | head -1)
cp "$input" io/InCollege-Input.txt
./InCollege
cat io/InCollege-Output.txt
