#!/bin/bash
# Simple example-suite runner for the Rust crate.

set -e

cd "$(dirname "$0")"
ROOT="$(pwd)"
EXAMPLES_DIR="$ROOT/../examples"
TERSMU="$ROOT/target/release/tersmu"

if [ ! -f "$TERSMU" ]; then
    echo "Error: tersmu binary not found at $TERSMU"
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

    if [ ! -f "$EXAMPLES_DIR/$i.loj" ]; then
        echo "SKIP (no .loj file)"
        continue
    fi

    ACTUAL=$(mktemp)
    "$TERSMU" -L < "$EXAMPLES_DIR/$i.jbo" 2>&1 | grep -v "^DEBUG" > "$ACTUAL" || true

    if diff -q "$EXAMPLES_DIR/$i.loj" "$ACTUAL" > /dev/null 2>&1; then
        echo "PASS"
        PASSED=$((PASSED + 1))
    else
        echo "FAIL"
        FAILED=$((FAILED + 1))
        DIFFS+=("$i")
        if [ -z "$DIFF_DIR" ]; then
            DIFF_DIR=$(mktemp -d)
        fi
        diff -u "$EXAMPLES_DIR/$i.loj" "$ACTUAL" > "$DIFF_DIR/$i.diff" 2>&1 || true
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
