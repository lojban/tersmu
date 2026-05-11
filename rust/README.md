# tersmu Rust

This directory contains the Rust implementation of `tersmu`, a semantic parser for Lojban that translates text into predicate-logic-style output and canonicalized Lojban.

The Haskell implementation in the repository root remains the source of truth. The Rust code is a direct port where possible, with the integrated camxes PEG parser used as the Rust parser engine.

## Prerequisites

- Rust toolchain with Cargo.
- For the browser app only:
  - `rustup target add wasm32-unknown-unknown`
  - `cargo install wasm-bindgen-cli`

## Build

From this directory:

```bash
cargo build --release --bin tersmu
```

From the repository root:

```bash
cargo build --release --bin tersmu --manifest-path rust/Cargo.toml
```

The release binary is written to `target/release/tersmu` under this directory.

## Run

```bash
# Parse a file, one line per Lojban text
./target/release/tersmu -L ../examples/1.jbo

# Parse from stdin
echo "mi klama le zarci" | ./target/release/tersmu -L

# Output JSON lines
echo "mi klama le zarci" | ./target/release/tersmu --json -

# Logical form only
echo "mi klama le zarci" | ./target/release/tersmu -l -L

# Canonical Lojban only
echo "mi klama le zarci" | ./target/release/tersmu -j -L
```

Useful options:

| Option | Description |
| --- | --- |
| `-l`, `--loj` | Output logical form only |
| `-j`, `--jbo` | Output forethoughtful/canonical Lojban only |
| `-L`, `--lines` | Treat each input line as a separate text |
| `-p`, `--paragraphs` | Treat blank-line-separated blocks as texts |
| `-u`, `--utf8` | Output UTF-8 symbols |
| `--json` | Output one JSON object per parsed text |

Successful JSON output has this shape:

```json
{"input":"mi klama le zarci","logical":"...","canonical":"...","graph":{"format":"graph","nodes":[],"edges":[]},"error":null}
```

On failure, `logical`, `canonical`, and `graph` are `null`, and `error` contains the morphology or parse error.

## Test

The golden examples live in `../examples` and are read-only test inputs. The scripts write temporary output outside that directory.

```bash
cargo build --release --bin tersmu
./test_all_examples.sh
```

Focused validation:

```bash
cargo test --test camxes_lojban_grammar
cargo test --test camxes_semantic_actions
cargo build --examples
cargo bench --bench camxes_parser --no-run
```

The camxes cmaxes example uses TSV data from `tests/data/` by default:

```bash
cargo run --example camxes_cmaxes_test
# or explicitly
cargo run --example camxes_cmaxes_test -- tests/data/lujvo_tests.tsv
```

`cargo test` also includes parser-generator tooling tests. Those are useful for `pappyc` development, but the core `tersmu` validation path is the release build, golden examples, focused camxes tests, examples build, and benchmark compile above.

## WebAssembly browser app

Build the Rust WASM package:

```bash
./build_wasm.sh
```

Generated wasm-bindgen files are written to `web-app/static/pkg/` and are ignored as build artifacts.

Run the static app locally:

```bash
cd web-app/static
python3 -m http.server 8000
```

Open `http://127.0.0.1:8000/?q=mi%20klama%20le%20zarci`.

For browser validation with Playwright MCP, confirm:

- no console errors;
- `window.initWasm` and `window.tersmuParse` exist;
- logical output includes `klama(mi,c0)`;
- canonical output includes `mi klama cy no`;
- graph JSON has `format: "graph"` and non-empty nodes/edges;
- the results panel is visible and the error panel is hidden.

See [web-app/README.md](web-app/README.md) for the app-specific notes.

## Project layout

```text
rust/
  Cargo.toml
  src/
    main.rs              # CLI entry point
    run.rs               # line-oriented parse/JSON orchestration
    wasm.rs              # wasm-bindgen entry point
    camxes/              # integrated PEG parser and Lojban grammar
      grammar/lojban.peg
      peg/
    pappy/               # dev parser runtime/reference code
    pappyc/              # dev parser compiler/reference code
    jbo_*.rs             # semantic parse, proposition, display, graph modules
    morphology*.rs       # morphology/preparse integration
  examples/              # Cargo examples for debugging/profiling
  tests/                 # Integration tests and test data
  benches/               # Criterion benchmarks
  web-app/               # Rust WASM browser app
  docs/                  # Rust-specific notes
```

## camxes

The former standalone `camxes-rs` crate is integrated as `tersmu::camxes`. The embedded Lojban PEG grammar is `src/camxes/grammar/lojban.peg`, exposed through `tersmu::camxes::LOJBAN_GRAMMAR`.

Use the focused tests when changing camxes parser behavior or the embedded grammar:

```bash
cargo test --test camxes_lojban_grammar
cargo test --test camxes_semantic_actions
```

## pappy and pappyc

`src/pappy/` and `src/pappyc/` are development/parser-generation tooling and historical reference ports of the Haskell Pappy infrastructure. They are not the runtime parser path for `tersmu`; the Rust runtime uses the integrated camxes parser.

The `pappyc` binary can still be built for grammar compiler work:

```bash
cargo build --bin pappyc
```

Do not treat `pappyc` as required for normal `tersmu` parsing, golden validation, or WASM builds.

## Additional documentation

- [rust-plan.md](rust-plan.md) — Rust port roadmap and validation rules.
- [HASKELL_VS_RUST_COMPARISON.md](HASKELL_VS_RUST_COMPARISON.md) — current parity status.
- [CAMXES-BUG-REPORT.md](CAMXES-BUG-REPORT.md) — camxes integration status and historical bug notes.
- [docs/camxes-integration.md](docs/camxes-integration.md) — current camxes module notes.
- [docs/camxes-plan-archive.md](docs/camxes-plan-archive.md) — archived standalone camxes plan.
- [../docs/](../docs/) — repository-level design notes and historical documentation.

## License

GPL-3.0. See [../COPYING](../COPYING).
