#!/bin/bash
# Validate the REST API using the same example inputs and expected .loj outputs.
# Run the container with: docker run -d -p 8080:8080 --name tersmu-api tersmu
# Then: ./test_api_examples.sh
# Or use: docker run --rm -p 8080:8080 tersmu & sleep 5 && ./test_api_examples.sh

set -e

cd "$(dirname "$0")"

API_URL="${TERSMU_API_URL:-http://localhost:8080}"
TOTAL=0
PASSED=0
FAILED=0
DIFFS=()

echo "Testing REST API ($API_URL) with examples..."
echo "============================================="
echo ""

# Optional: start container if not running
if [ -z "$SKIP_START" ] && ! curl -s -o /dev/null -w "%{http_code}" "$API_URL/health" 2>/dev/null | grep -q 200; then
  echo "API not reachable at $API_URL. Start the container with:"
  echo "  docker run -d -p 8080:8080 --name tersmu-api tersmu"
  echo "Or run in foreground: docker run --rm -p 8080:8080 tersmu"
  exit 1
fi

for i in {1..19}; do
  if [ ! -f "examples/$i.jbo" ]; then
    continue
  fi
  if [ ! -f "examples/$i.loj" ]; then
    continue
  fi

  TOTAL=$((TOTAL + 1))
  echo -n "Testing example $i... "

  EXPECTED="examples/$i.loj"
  ACTUAL=$(mktemp)

  # POST file body to /parse and compare to expected
  if curl -s -X POST -H "Content-Type: text/plain; charset=utf-8" \
    --data-binary @"examples/$i.jbo" \
    "$API_URL/parse" > "$ACTUAL"; then
    if diff -u "$EXPECTED" "$ACTUAL" > /dev/null 2>&1; then
      echo "PASS"
      PASSED=$((PASSED + 1))
    else
      echo "FAIL"
      FAILED=$((FAILED + 1))
      DIFFS+=("$i")
      diff -u "$EXPECTED" "$ACTUAL" > "examples/$i.api.diff" || true
    fi
  else
    echo "FAIL (curl error)"
    FAILED=$((FAILED + 1))
    DIFFS+=("$i")
  fi

  rm -f "$ACTUAL"
done

echo ""
echo "============================================="
echo "Results: $PASSED/$TOTAL passed, $FAILED failed"

if [ $FAILED -gt 0 ]; then
  echo ""
  echo "Failed examples: ${DIFFS[*]}"
  echo "Check examples/*.api.diff for details"
  exit 1
else
  echo "All API tests passed!"
  exit 0
fi
