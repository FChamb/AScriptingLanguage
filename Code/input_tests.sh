#!/bin/bash

# Automated user input testing
# TODO: Probably much easier to provide a flag to echo input / Use file input

cabal build H2 -v0 || (echo "Build failure" && exit 1)

PID="$$"
TEMP_OUT_FILE=".test_out"

function write {
    # Wait enough time for the program to write to the file
    sleep 0.2; echo "$1" | tee -a "$TEMP_OUT_FILE"
}


function runTest {
    name="$1"
    inputFunc="$2"
    expectedOut="$3"
    echo "TEST: $1"
    >"$TEMP_OUT_FILE"
    $inputFunc | cabal run H2 -v0 >> "$TEMP_OUT_FILE"
    sleep 0.3
    output=$(cat "$TEMP_OUT_FILE")
    if [ "$output" = "$expectedOut" ]; then
        echo "==== TEST $1: PASS ==="
        cat "$TEMP_OUT_FILE"
        rm "$TEMP_OUT_FILE"
        return 0
    else
        echo "===== TEST $1: FAIL ===="
        echo "= EXPECTED VS ACTUAL ="
        diff -y --color <(echo "$expectedOut") "$TEMP_OUT_FILE"
        rm "$TEMP_OUT_FILE"
        return 1
    fi
}

expectedBasicPrintVar="""> a=1+2
> print a
3
> EOF, goodbye"""

function basicPrintVar {
    write "a=1+2"
    write "print a"
}

expectedBasicInput="""> test = input
hello world
> print test
hello world
> EOF, goodbye"""

function basicInput {
    write "test = input"
    write "hello world"
    write "print test"
}

expectedAddMul="""> print 2+4*6
26
> EOF, goodbye"""

function addMul {
    write "print 2+4*6"
}

expectedAddMulBracketed="""> print (2+4)*6
36
> EOF, goodbye"""

function addMulBracketed {
    write "print (2+4)*6"
}

echo "Preparing to run"
#| grep -v -e "^\\[.*\\]$")
failed=0
runTest "basicPrintVar" basicPrintVar "$expectedBasicPrintVar" || ((failed++))
runTest "basicInput" basicInput "$expectedBasicInput" || ((failed++))
runTest "addMul" addMul "$expectedAddMul" || ((failed++))
runTest "addMulBracketed" addMulBracketed "$expectedAddMulBracketed" || ((failed++))

if [ "$failed" -eq "0" ]; then
    echo "All tests passed"
else
    echo "$failed tests failed"
    exit 1
fi
