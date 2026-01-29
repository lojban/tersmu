# Changelog
 
## 0.2.3

- **Infrastructure**: Docker builds with multi-stage layers and dependency caching. Removed nix support.
- **Full support for Docker development**: Added `Dockerfile.dev` and `docker-compose.dev.yml` with hot reloading (via `ghcid`).
- **WebAssembly target**: Added support for compiling to WASM and a new modern web application interface.
- **Improved REST API**: Enhanced JSON output (NDJSON support), better non-ASCII character handling, and health check endpoints.
- **Build system overhaul**: Automated `.pappy` generation from `.pest` grammars via Python scripts.
- **Cleanup**: Removed legacy IRC bot and modernized documentation.

## 0.2.2

- Support for GHC 7.10
- Miscellaneous fixes

## 0.2

- First release on Hackage
