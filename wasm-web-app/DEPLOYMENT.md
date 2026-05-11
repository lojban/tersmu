# tersmu WASM Web App - Deployment

This directory contains the build configuration for deploying the tersmu Lojban parser as a WebAssembly application to GitHub Pages.

## Features

- **Dual Implementation**: Switch between Haskell and Rust implementations via tabs
- **Real-time Parsing**: Parse Lojban text directly in your browser
- **Semantic Graph Visualization**: Interactive graph rendering with multiple layout algorithms
- **No Server Required**: Runs entirely client-side using WebAssembly

## Architecture

### Build Pipeline

The GitHub Actions workflow (`.github/workflows/deploy-wasm.yml`) performs the following:

1. **Haskell WASM Build**
   - Uses Docker with GHC's WebAssembly backend
   - Builds from `Dockerfile.wasm`
   - Outputs: `tersmu-haskell.wasm`

2. **Rust WASM Build**
   - Uses `wasm-bindgen` for JavaScript interop
   - Compiles to `wasm32-unknown-unknown` target
   - Outputs: `tersmu-rust.js` and `tersmu-rust_bg.wasm`

3. **Deployment**
   - Combines both implementations
   - Deploys to GitHub Pages (`gh-pages` branch)
   - Accessible at: `https://<username>.github.io/<repo>/`

### File Structure

```
deploy/
├── index.html                  # Main web interface with tab switcher
├── tersmu-haskell.wasm         # Haskell implementation
├── tersmu-haskell.js           # Haskell WASM wrapper
├── tersmu-rust.js              # Rust wasm-bindgen generated JS
├── tersmu-rust_bg.wasm         # Rust implementation
├── tersmu-rust-wrapper.js      # Rust unified interface wrapper
├── tree-viz.js                 # Graph visualization
└── favicon.svg                 # Site icon
```

## Local Development

### Building Haskell WASM

```bash
./build_wasm.sh
```

### Building Rust WASM

```bash
cd rust
cargo build --release --target wasm32-unknown-unknown --lib
wasm-bindgen \
  --target web \
  --out-dir ../wasm-web-app \
  --out-name tersmu-rust \
  target/wasm32-unknown-unknown/release/tersmu.wasm
```

### Testing Locally

```bash
cd wasm-web-app
python3 -m http.server 8000
# Open http://localhost:8000
```

## Implementation Switching

The web interface includes a tab bar in the header to switch between implementations:

- **Haskell**: Original implementation using GHC WASM backend
- **Rust**: New Rust port with wasm-bindgen

Both implementations provide the same JSON output format for consistency.

## GitHub Pages Configuration

### Enable GitHub Pages

1. Go to repository Settings → Pages
2. Source: Deploy from a branch
3. Branch: `gh-pages` / `root`
4. Save

The workflow automatically deploys on push to `master`.

## Browser Compatibility

- Chrome/Edge 57+
- Firefox 52+
- Safari 11+

Requires WebAssembly support.

## Performance

- **Haskell WASM**: ~9MB (includes GHC runtime)
- **Rust WASM**: ~200KB (optimized with wasm-bindgen)

The Rust implementation loads significantly faster but both provide equivalent parsing functionality.
