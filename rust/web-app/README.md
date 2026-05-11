# Rust WASM web app

This is the Rust-backed browser app for `tersmu`. It uses the Rust crate's `wasm-bindgen` export instead of the legacy Haskell WASM runtime.

## Build

From this directory's parent (`rust/`):

```bash
./build_wasm.sh
```

If prerequisites are missing, install them with:

```bash
rustup target add wasm32-unknown-unknown
cargo install wasm-bindgen-cli
```

## Run locally

After building:

```bash
cd web-app/static
python3 -m http.server 8000
```

Open `http://localhost:8000/` and parse a sentence such as `mi klama le zarci`.

The generated `static/pkg/` directory is build output and is ignored by git. Re-run `./build_wasm.sh` after Rust parser or WASM entry-point changes.
