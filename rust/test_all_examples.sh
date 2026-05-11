#!/bin/bash
# Test script to validate all examples against expected output from the Rust crate.

set -e

cd "$(dirname "$0")"
ROOT="$(pwd)"
EXAMPLES_DIR="$ROOT/../examples"

PARSER_BIN="$ROOT/target/release/tersmu"

if [ ! -f "$PARSER_BIN" ]; then
    echo "Error: tersmu binary not found at $PARSER_BIN"
    echo "Run: cargo build --release --bin tersmu"
    exit 1
fi

TOTAL=0
PASSED=0
FAILED=0
DIFFS=()
DIFF_DIR=""

echo "Testing all examples..."
echo "======================"
echo ""

for i in {1..20}; do
    if [ ! -f "$EXAMPLES_DIR/$i.jbo" ]; then
        continue
    fi

    TOTAL=$((TOTAL + 1))
    echo -n "Testing example $i... "

    if [ -f "$EXAMPLES_DIR/$i.loj" ]; then
        EXPECTED="$EXAMPLES_DIR/$i.loj"
    else
        echo "SKIP (no .loj file)"
        continue
    fi

    ACTUAL=$(mktemp)
    "$PARSER_BIN" -L < "$EXAMPLES_DIR/$i.jbo" 2>&1 | grep -v "^DEBUG" > "$ACTUAL" || true

    if diff -u "$EXPECTED" "$ACTUAL" > /dev/null 2>&1; then
        echo "PASS"
        PASSED=$((PASSED + 1))
    else
        echo "FAIL"
        FAILED=$((FAILED + 1))
        DIFFS+=("$i")
        if [ -z "$DIFF_DIR" ]; then
            DIFF_DIR=$(mktemp -d)
        fi
        diff -u "$EXPECTED" "$ACTUAL" > "$DIFF_DIR/$i.diff" || true
    fi

    rm -f "$ACTUAL"
done

echo ""
echo "======================"
echo "Results: $PASSED/$TOTAL passed, $FAILED failed"

if [ $FAILED -gt 0 ]; then
    echo ""
    echo "Failed examples: ${DIFFS[*]}"
    echo "Check $DIFF_DIR/*.diff files for details"
    exit 1
else
    echo "All tests passed!"
    exit 0
fi
