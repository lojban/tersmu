#!/bin/bash
echo "Testing morphology for: $1"
cat > /tmp/test_input.txt << INNER
$1
INNER
./target/release/tersmu /tmp/test_input.txt 2>&1
