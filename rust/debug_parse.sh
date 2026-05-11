#!/bin/bash
echo "Testing: $1"
echo "$1" | ./target/release/tersmu - 2>&1
echo "---"
