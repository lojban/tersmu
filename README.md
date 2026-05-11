# tersmu - Lojban Semantic Parser

A powerful semantic parser for Lojban that converts Lojban text into logical forms and canonical representations.

## 🌐 Try it Online

**Live Demo:** [https://lojban.github.io/tersmu/](https://lojban.github.io/tersmu/) *(or your GitHub Pages URL)*

The web application runs entirely in your browser using WebAssembly, with two implementations to choose from:
- **Haskell**: Original implementation
- **Rust**: High-performance port

## Features

- **Semantic Parsing**: Converts Lojban text to logical forms
- **Canonical Form**: Generates normalized Lojban output
- **Graph Visualization**: Interactive semantic graph rendering
- **Dual Implementation**: Switch between Haskell and Rust parsers
- **Browser-Based**: No server required, runs via WebAssembly

## Quick Start

### Web Application

Visit the [live demo](https://lojban.github.io/tersmu/) or run locally:

```bash
cd wasm-web-app
python3 -m http.server 8000
# Open http://localhost:8000
```

### Command Line (Rust)

```bash
cd rust
cargo build --release
./target/release/tersmu "mi klama le zarci"
```

### Command Line (Haskell)

```bash
cabal build
cabal run tersmu -- "mi klama le zarci"
```

## Building

### Prerequisites

**For Haskell:**
- GHC 9.x
- Cabal 3.x
- Docker (for WASM builds)

**For Rust:**
- Rust 1.70+
- wasm-bindgen-cli (for WASM builds)

### Build WASM Applications

**Haskell WASM:**
```bash
./build_wasm.sh
```

**Rust WASM:**
```bash
cd rust
cargo build --release --target wasm32-unknown-unknown --lib
wasm-bindgen \
  --target web \
  --out-dir ../wasm-web-app \
  --out-name tersmu-rust \
  target/wasm32-unknown-unknown/release/tersmu.wasm
```

## CI/CD Pipeline

The project uses GitHub Actions to automatically build and deploy both implementations to GitHub Pages.

**Workflow:** `.github/workflows/deploy-wasm.yml`

**On every push to `master`:**
1. Builds Haskell WASM (~9MB)
2. Builds Rust WASM (~200KB)
3. Packages web application
4. Deploys to GitHub Pages

See [CI/CD Documentation](.github/CI-CD.md) for details.

## Project Structure

```
tersmu/
├── .github/
│   ├── workflows/
│   │   └── deploy-wasm.yml      # CI/CD pipeline
│   └── CI-CD.md                 # Pipeline documentation
├── rust/                        # Rust implementation
│   ├── src/
│   │   ├── lib.rs
│   │   ├── wasm.rs             # WASM bindings
│   │   └── ...
│   └── Cargo.toml
├── wasm-web-app/               # Web application
│   ├── index.html              # Main UI with tab switcher
│   ├── tersmu.js               # Haskell WASM wrapper
│   ├── tersmu-rust-wrapper.js  # Rust WASM wrapper
│   ├── tree-viz.js             # Graph visualization
│   ├── DEPLOYMENT.md           # Deployment guide
│   └── .nojekyll               # GitHub Pages config
├── Dockerfile.wasm             # Haskell WASM build
├── build_wasm.sh               # Haskell build script
└── tersmu.cabal                # Haskell project file
```

## Development

### Running Tests

**Rust:**
```bash
cd rust
cargo test
```

**Haskell:**
```bash
cabal test
```

### Local Development Server

```bash
cd wasm-web-app
python3 -m http.server 8000
```

## Implementation Comparison

| Feature | Haskell | Rust |
|---------|---------|------|
| WASM Size | ~9 MB | ~200 KB |
| Load Time | 3-5 sec | <1 sec |
| Build Time | 15-20 min | 2-3 min |
| Parse Speed | Fast | Fast |
| Maturity | Original | Port |

Both implementations produce identical output.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests
5. Submit a pull request

The CI/CD pipeline will automatically build and test your changes.

## Documentation

- [CI/CD Pipeline](.github/CI-CD.md) - Automated build and deployment
- [Deployment Guide](wasm-web-app/DEPLOYMENT.md) - GitHub Pages setup
- [Rust Implementation](rust/README.md) - Rust-specific documentation

## Browser Compatibility

- Chrome/Edge 57+
- Firefox 52+
- Safari 11+

Requires WebAssembly support.

## License

GPL-3.0

## Links

- [Lojban.org](https://lojban.org) - Official Lojban website
- [Lojban Wiki](https://mw.lojban.org) - Community wiki
- [Discord](https://discord.gg/4KhzRzpmVr) - Live chat

## Acknowledgments

Built with:
- [GHC WebAssembly Backend](https://gitlab.haskell.org/ghc/ghc-wasm-meta)
- [wasm-bindgen](https://rustwasm.github.io/wasm-bindgen/)
- [Cytoscape.js](https://js.cytoscape.org/) - Graph visualization
