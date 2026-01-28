#!/bin/bash
# Test script to validate all examples against expected output

set -e

cd "$(dirname "$0")"

# Prefer the Haskell binary if available, otherwise fall back to the Rust implementation.
# This lets us validate examples against the original Haskell parser while still
# supporting the Rust reimplementation in development.
if command -v tersmu >/dev/null 2>&1; then
    PARSER_BIN="tersmu"
elif [ -f "rust/target/release/tersmu" ]; then
    PARSER_BIN="rust/target/release/tersmu"
else
    PARSER_BIN="cargo run --bin tersmu --manifest-path rust/Cargo.toml --release --"
fi
TOTAL=0
PASSED=0
FAILED=0
DIFFS=()

echo "Testing all examples..."
echo "======================"
echo ""

for i in {1..19}; do
    if [ ! -f "examples/$i.jbo" ]; then
        continue
    fi
    
    TOTAL=$((TOTAL + 1))
    echo -n "Testing example $i... "
    
    # Get expected output
    if [ -f "examples/$i.loj" ]; then
        EXPECTED="examples/$i.loj"
    else
        echo "SKIP (no .loj file)"
        continue
    fi
    
    # Get actual output (suppress warnings and compilation messages)
    ACTUAL=$(mktemp)
    $PARSER_BIN "examples/$i.jbo" 2>/dev/null > "$ACTUAL" || \
    $PARSER_BIN "examples/$i.jbo" 2>&1 | grep -v "^warning:" | grep -v "Compiling" | grep -v "Finished" | grep -v "Running" | grep -v "^   -->" | grep -v "note:" | grep -v "help:" | grep -v "^   |" | grep -v "^    |" | grep -v "= note:" | grep -v "warning:" | grep -v "error:" | grep -v "^$" | grep -v "^[[:space:]]*$" > "$ACTUAL" || true
    
    # Compare (ignore whitespace differences at end of lines)
    if diff -u "$EXPECTED" "$ACTUAL" > /dev/null 2>&1; then
        echo "PASS"
        PASSED=$((PASSED + 1))
    else
        echo "FAIL"
        FAILED=$((FAILED + 1))
        DIFFS+=("$i")
        # Save diff for later
        diff -u "$EXPECTED" "$ACTUAL" > "examples/$i.diff" || true
    fi
    
    rm -f "$ACTUAL"
done

echo ""
echo "======================"
echo "Results: $PASSED/$TOTAL passed, $FAILED failed"

if [ $FAILED -gt 0 ]; then
    echo ""
    echo "Failed examples: ${DIFFS[*]}"
    echo "Check examples/*.diff files for details"
    exit 1
else
    echo "All tests passed!"
    exit 0
fi
