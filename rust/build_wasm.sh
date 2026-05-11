#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

if ! rustup target list --installed | grep -qx 'wasm32-unknown-unknown'; then
    echo "Missing Rust target: wasm32-unknown-unknown"
    echo "Install it with: rustup target add wasm32-unknown-unknown"
    exit 1
fi

if ! command -v wasm-bindgen >/dev/null 2>&1; then
    echo "Missing wasm-bindgen CLI"
    echo "Install it with: cargo install wasm-bindgen-cli"
    exit 1
fi

cargo build --lib --release --target wasm32-unknown-unknown --manifest-path Cargo.toml
wasm-bindgen \
    --target web \
    --out-dir web-app/static/pkg \
    target/wasm32-unknown-unknown/release/tersmu.wasm

if command -v wasm-opt >/dev/null 2>&1; then
    wasm-opt -Oz -o web-app/static/pkg/tersmu_bg.wasm web-app/static/pkg/tersmu_bg.wasm
fi

echo "Rust WASM web app built in web-app/static/pkg"
