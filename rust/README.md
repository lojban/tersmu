# camxes-rs

A comprehensive Lojban parser combining fast PEG parsing with semantic analysis capabilities.

**camxes-rs** provides both low-level parsing (via the integrated `camxes` PEG parser) and high-level semantic analysis (via the `tersmu` semantic engine). Use it as a standalone parser library or as a complete semantic analyzer.

## Features

- **Fast PEG Parser**: Zero-copy parsing with span-based tokens
- **Semantic Analysis**: Converts Lojban to logical forms and canonical representations
- **Rich Error Diagnostics**: Position tracking and detailed error messages
- **WebAssembly Support**: Runs in browsers via WASM
- **Thread-Safe**: Create parser instances per thread for concurrent usage
- **Comprehensive Testing**: Validated against extensive golden examples

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
camxes-rs = "1.0"
```

## Quick Start

### As a PEG Parser

Use the integrated `camxes` module for fast, low-level parsing:

```rust
use camxes_rs::camxes::peg::grammar::Peg;
use camxes_rs::camxes::LOJBAN_GRAMMAR;

fn main() {
    // Create parser from embedded Lojban grammar
    let (start_rule, grammar_text) = LOJBAN_GRAMMAR;
    let parser = Peg::new(start_rule, grammar_text).expect("Failed to build parser");
    
    // Parse Lojban text
    let input = "mi klama le zarci";
    let result = parser.parse(input);
    
    // result is ParseResult(cost, consumed_pos, error_pos, Result<Vec<ParseNode>, ParseError>)
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

### With Semantic Analysis

Use the high-level API for logical forms and canonical output:

```rust
use camxes_rs::parse_lojban::parse_text;

fn main() {
    env_logger::init();  // Optional: enable logging
    
    let result = parse_text("mi klama le zarci");
    match result {
        Ok((logical, canonical, graph)) => {
            println!("Logical form: {}", logical);
            println!("Canonical: {}", canonical);
            // graph contains semantic graph structure
        }
        Err(e) => eprintln!("Parse error: {}", e),
    }
}
```

## Command-Line Tool

The crate includes a `camxes` binary for command-line usage:

```bash
# Install
cargo install camxes-rs

# Parse a file (one sentence per line)
camxes -L input.jbo

# Parse from stdin
echo "mi klama le zarci" | camxes -L

# Output JSON
echo "mi klama le zarci" | camxes --json -

# Logical form only
echo "mi klama le zarci" | camxes -l -L

# Canonical Lojban only
echo "mi klama le zarci" | camxes -j -L
```

### Logging

Enable debug output with the `RUST_LOG` environment variable:

```bash
# All debug logs
RUST_LOG=debug camxes -L input.jbo

# Only camxes-rs logs
RUST_LOG=camxes_rs=debug camxes -L input.jbo

# Specific module logs
RUST_LOG=camxes_rs::morphology=debug,camxes_rs::parse_lojban=trace camxes -L input.jbo
```

## API Examples

### Token Extraction

Extract tokens with text spans:

```rust
use camxes_rs::camxes::peg::grammar::Peg;
use camxes_rs::camxes::peg::parsing::{ParseNode, ParseResult};
use camxes_rs::camxes::LOJBAN_GRAMMAR;

#[derive(Debug)]
struct Token {
    name: String,
    text: String,
    start: usize,
    end: usize,
}

impl Token {
    fn from_parse_node(node: &ParseNode, input: &str) -> Self {
        match node {
            ParseNode::Terminal { name, start, end } => Token {
                name: name.clone(),
                text: input[*start..*end].to_string(),
                start: *start,
                end: *end,
            },
            ParseNode::NonTerminal { name, start, end, .. } => Token {
                name: name.clone(),
                text: input[*start..*end].to_string(),
                start: *start,
                end: *end,
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
```

### Custom Grammar Rules

Parse specific syntactic constructs:

```rust
use camxes_rs::camxes::peg::grammar::Peg;
use camxes_rs::camxes::LOJBAN_GRAMMAR;

// Parse only a word (morphology level)
let (_, grammar) = LOJBAN_GRAMMAR;
let word_parser = Peg::new("lojban_word", grammar)?;
let result = word_parser.parse("klama");

// Parse a specific construct
let sumti_parser = Peg::new("sumti", grammar)?;
let result = sumti_parser.parse("le zarci");
```

### Multi-threaded Usage

For web servers or concurrent applications:

```rust
use std::collections::HashMap;
use std::sync::Arc;
use camxes_rs::camxes::peg::grammar::Peg;
use camxes_rs::camxes::LOJBAN_GRAMMAR;

// In server initialization
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

## API Compatibility Note

**Important:** The embedded `camxes` module differs from the standalone `camxes-rs` 0.1.x:

- **0.1.x**: `ParseResult(cost, position, result)` - result at index 2
- **1.0.0+**: `ParseResult(cost, position, error_position, result)` - result at index 3

The 1.0.0+ version adds an explicit error position field for better error diagnostics. When migrating from 0.1.x, change `result.2` to `result.3` to access the parse result.

## WebAssembly

Build for WASM:

```bash
cargo build --release --target wasm32-unknown-unknown --lib
wasm-bindgen \
  --target web \
  --out-dir ./pkg \
  --out-name camxes \
  target/wasm32-unknown-unknown/release/camxes_rs.wasm
```

See the [web-app](web-app/) directory for a complete browser example.

## Development

### Build

```bash
cargo build --release
```

### Test

```bash
# Run all tests
cargo test

# Run focused camxes tests
cargo test --test camxes_lojban_grammar
cargo test --test camxes_semantic_actions

# Build examples
cargo build --examples

# Run benchmarks
cargo bench --bench camxes_parser
```

### Project Structure

```
rust/
  Cargo.toml
  src/
    lib.rs               # Crate root with documentation
    main.rs              # CLI entry point
    camxes/              # Integrated PEG parser
      grammar/lojban.peg # Embedded Lojban grammar
      peg/               # PEG parser engine
    parse_lojban.rs      # High-level semantic API
    morphology.rs        # Morphology validation
    jbo_*.rs             # Semantic analysis modules
  examples/              # Usage examples
  tests/                 # Integration tests
  benches/               # Performance benchmarks
  web-app/               # WASM browser application
```

## Modules

- **`camxes`**: PEG parser with embedded Lojban grammar
- **`parse_lojban`**: High-level semantic parsing API
- **`morphology`**: Lojban morphology validation
- **`jbo_tree`**, **`jbo_syntax`**, **`jbo_prop`**: Semantic tree structures
- **`jbo_show`**: Output formatting (logical forms, canonical Lojban)
- **`jbo_parse`**: Parse tree to semantic tree conversion
- **`run`**: CLI orchestration and JSON output

## Documentation

- [API Documentation](https://docs.rs/camxes-rs) - Full API reference
- [Repository](https://github.com/lojban/tersmu) - Source code and examples
- [Lojban.org](https://lojban.org) - Official Lojban website

## License

GPL-3.0 - See [LICENSE](../COPYING) for details.

## Acknowledgments

This crate combines:
- **camxes**: PEG parser originally developed as a standalone crate
- **tersmu**: Semantic analysis engine (Rust port of the Haskell implementation)

Both are now integrated into a single, comprehensive Lojban parsing library.
