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

### Logging

The `tersmu` library uses the `log` crate for internal diagnostics. To enable logging output when using the CLI or integrating the library:

```bash
# Enable all debug logs
RUST_LOG=debug ./target/release/tersmu -L input.jbo

# Enable only tersmu logs at debug level
RUST_LOG=tersmu=debug ./target/release/tersmu -L input.jbo

# Enable specific module logs
RUST_LOG=tersmu::morphology=debug,tersmu::parse_lojban=trace ./target/release/tersmu -L input.jbo
```

When using `tersmu` as a library dependency, initialize a logger in your application:

```rust
use tersmu::parse_lojban::parse_text;

fn main() {
    env_logger::init();  // or any other log implementation
    
    let result = parse_text("mi klama le zarci");
    // Internal tersmu diagnostics will be logged based on RUST_LOG
}
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

## Using tersmu as a camxes parser library

The `tersmu` crate includes the integrated `camxes` PEG parser, which can be used independently for Lojban parsing without the full semantic analysis pipeline.

### API Compatibility Note

**Important:** The embedded `tersmu::camxes` API differs from the standalone `camxes-rs` crate in one critical way:

- **camxes-rs:** `ParseResult(cost, position, result)` - result at index 2
- **tersmu::camxes:** `ParseResult(cost, position, error_position, result)` - result at index 3

The tersmu version adds an explicit error position field for better error diagnostics. When migrating from `camxes-rs`, change `result.2` to `result.3` to access the parse result.

### Basic camxes usage

```rust
use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::LOJBAN_GRAMMAR;

fn main() {
    // Create a parser from the embedded Lojban grammar
    let (start_rule, grammar_text) = LOJBAN_GRAMMAR;
    let parser = Peg::new(start_rule, grammar_text).expect("Failed to build parser");
    
    // Parse Lojban text
    let input = "mi klama le zarci";
    let result = parser.parse(input);
    
    // result is a ParseResult(cost, consumed_pos, error_pos, Result<Vec<ParseNode>, ParseError>)
    match result.3.as_ref() {
        Ok(nodes) => {
            println!("Parse succeeded!");
            for node in nodes {
                println!("{:?}", node);
            }
        }
        Err(err) => {
            println!("Parse failed at position {}: {:?}", err.position, err);
        }
    }
}
```

### Type signatures

```rust
use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::peg::parsing::{ParseResult, ParseNode, ParseError};

// Create a parser for a specific grammar rule
let parser: Peg = Peg::new("text", grammar_text)?;

// Parse input and get results
let ParseResult(cost: u32, consumed: usize, error_pos: usize, result: Arc<Result<Vec<ParseNode>, ParseError>>) 
    = parser.parse(input);

// ParseNode is an enum with Terminal and NonTerminal variants
match node {
    ParseNode::Terminal { name, start, end } => {
        let text = &input[start..end];
        // Process terminal token
    }
    ParseNode::NonTerminal { name, start, end, children } => {
        // Process non-terminal with children
        for child in children {
            // Recursively process
        }
    }
}
```

### Advanced: Custom grammar rules

You can also parse with custom PEG rules or different entry points:

```rust
use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::LOJBAN_GRAMMAR;

// Parse only a word (morphology level)
let (_, grammar) = LOJBAN_GRAMMAR;
let word_parser = Peg::new("lojban_word", grammar)?;
let result = word_parser.parse("klama");

// Parse a specific syntactic construct
let sumti_parser = Peg::new("sumti", grammar)?;
let result = sumti_parser.parse("le zarci");
```

### Real-world example: Token extraction

This example shows how to extract tokens with their text spans (similar to lensisku usage):

```rust
use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::peg::parsing::{ParseNode, ParseResult};
use tersmu::camxes::LOJBAN_GRAMMAR;

#[derive(Debug)]
struct Token {
    name: String,
    text: String,
    start: usize,
    end: usize,
    children: Vec<Token>,
}

impl Token {
    fn from_parse_node(node: &ParseNode, input: &str) -> Self {
        match node {
            ParseNode::Terminal { name, start, end } => Token {
                name: name.clone(),
                text: input[*start..*end].to_string(),
                start: *start,
                end: *end,
                children: vec![],
            },
            ParseNode::NonTerminal { name, start, end, children } => Token {
                name: name.clone(),
                text: input[*start..*end].to_string(),
                start: *start,
                end: *end,
                children: children.iter().map(|c| Token::from_parse_node(c, input)).collect(),
            },
        }
    }
}

fn parse_to_tokens(input: &str) -> Result<Vec<Token>, String> {
    let (start_rule, grammar) = LOJBAN_GRAMMAR;
    let parser = Peg::new(start_rule, grammar)
        .map_err(|e| format!("Failed to build parser: {:?}", e))?;
    
    let ParseResult(_, _, _, result) = parser.parse(input);
    
    match result.as_ref() {
        Ok(nodes) => Ok(nodes.iter().map(|n| Token::from_parse_node(n, input)).collect()),
        Err(err) => Err(format!("Parse failed at position {}: {:?}", err.position, err)),
    }
}

fn main() {
    match parse_to_tokens("mi klama le zarci") {
        Ok(tokens) => {
            for token in tokens {
                println!("{}: {} [{}-{}]", token.name, token.text, token.start, token.end);
            }
        }
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

### Multi-threaded usage

For web servers or multi-threaded applications, create one `Peg` instance per thread (as in lensisku):

```rust
use std::collections::HashMap;
use std::sync::Arc;
use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::LOJBAN_GRAMMAR;

// In your server initialization
let grammar_texts: Arc<HashMap<i32, String>> = Arc::new({
    let mut map = HashMap::new();
    map.insert(1, LOJBAN_GRAMMAR.1.to_string()); // Language ID 1 = Lojban
    map
});

// In each worker thread
let mut parsers = HashMap::new();
for (lang_id, grammar_text) in grammar_texts.iter() {
    match Peg::new("text", grammar_text) {
        Ok(parser) => {
            parsers.insert(*lang_id, parser);
        }
        Err(e) => {
            log::error!("Failed to initialize parser for language {}: {}", lang_id, e);
        }
    }
}

// Use the parser
if let Some(parser) = parsers.get(&1) {
    let result = parser.parse("mi klama");
    // Process result...
}
```

## License

GPL-3.0. See [../COPYING](../COPYING).
