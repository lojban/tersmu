#!/bin/bash
# Build script for generating WASM web app from Docker
# This script builds the Docker image, extracts the WASM file, and creates a web app

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${SCRIPT_DIR}/wasm-web-app"
IMAGE_NAME="tersmu-wasm-builder"

echo "Building WASM Docker image..."
docker build -f Dockerfile.wasm -t ${IMAGE_NAME} .

echo "Creating output directory: ${OUTPUT_DIR}"
mkdir -p "${OUTPUT_DIR}"

echo "Extracting WASM file from Docker container..."
CONTAINER_ID=$(docker create ${IMAGE_NAME})
docker cp ${CONTAINER_ID}:/output/tersmu.wasm "${OUTPUT_DIR}/tersmu.wasm" || {
    echo "Warning: Could not extract tersmu.wasm, trying alternative location..."
    docker cp ${CONTAINER_ID}:/app/dist-newstyle "${OUTPUT_DIR}/dist-newstyle" 2>/dev/null || true
    find "${OUTPUT_DIR}/dist-newstyle" -name "*.wasm" -exec cp {} "${OUTPUT_DIR}/tersmu.wasm" \; 2>/dev/null || true
}
docker rm ${CONTAINER_ID}

if [ ! -f "${OUTPUT_DIR}/tersmu.wasm" ]; then
    echo "Error: WASM file not found after extraction"
    echo "Checking container contents..."
    docker run --rm ${IMAGE_NAME} find / -name "*.wasm" 2>/dev/null || true
    exit 1
fi

echo "WASM file extracted successfully: ${OUTPUT_DIR}/tersmu.wasm"
ls -lh "${OUTPUT_DIR}/tersmu.wasm"

echo "Copying web app files..."
[ "${SCRIPT_DIR}/wasm-web-app/index.html" != "${OUTPUT_DIR}/index.html" ] && cp "${SCRIPT_DIR}/wasm-web-app/index.html" "${OUTPUT_DIR}/index.html"
[ "${SCRIPT_DIR}/wasm-web-app/tersmu.js" != "${OUTPUT_DIR}/tersmu.js" ] && cp "${SCRIPT_DIR}/wasm-web-app/tersmu.js" "${OUTPUT_DIR}/tersmu.js"

echo ""
echo "Build complete! WASM web app is in: ${OUTPUT_DIR}/"
echo ""
echo "To test locally, you can use a simple HTTP server:"
echo "  cd ${OUTPUT_DIR}"
echo "  python3 -m http.server 8000"
echo "  Then open http://localhost:8000 in your browser"
echo ""
