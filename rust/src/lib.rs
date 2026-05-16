//! # camxes-rs: Lojban PEG Parser with Semantic Analysis
//!
//! `camxes-rs` is a comprehensive Lojban parser that combines:
//! - **PEG Parser**: Fast, zero-copy parsing with the integrated `camxes` module
//! - **Semantic Analysis**: Full tersmu semantic engine for logical form generation
//! - **Morphology**: Complete Lojban morphology validation
//! - **Multiple Outputs**: Parse trees, logical forms, canonical Lojban, and semantic graphs
//!
//! ## Quick Start
//!
//! ### As a PEG Parser (camxes)
//!
//! ```rust
//! use camxes_rs::camxes::peg::grammar::Peg;
//! use camxes_rs::camxes::LOJBAN_GRAMMAR;
//!
//! let (start_rule, grammar_text) = LOJBAN_GRAMMAR;
//! let parser = Peg::new(start_rule, grammar_text).expect("Failed to build parser");
//! let input = "mi klama le zarci";
//! let result = parser.parse(input);
//!
//! // result.3 contains Result<Vec<ParseNode>, ParseError>
//! match result.3.as_ref() {
//!     Ok(nodes) => println!("Parse succeeded with {} nodes", nodes.len()),
//!     Err(err) => eprintln!("Parse failed at position {}", err.position),
//! }
//! ```
//!
//! ### With Semantic Analysis
//!
//! ```rust
//! use camxes_rs::parse_lojban::parse_text;
//!
//! let result = parse_text("mi klama le zarci");
//! match result {
//!     Ok(text) => {
//!         println!("Parsed successfully: {:?}", text);
//!         // text is a Text struct containing the semantic parse tree
//!     }
//!     Err(pos) => eprintln!("Parse error at position: {}", pos),
//! }
//! ```
//!
//! ## Modules
//!
//! - [`camxes`]: PEG parser with embedded Lojban grammar
//! - [`parse_lojban`]: High-level semantic parsing API
//! - [`morphology`]: Lojban morphology validation
//! - [`jbo_tree`], [`jbo_syntax`], [`jbo_prop`]: Semantic tree structures
//! - [`jbo_show`]: Output formatting (logical forms, canonical Lojban)
//!
//! ## Features
//!
//! - Zero-copy parsing with span-based tokens
//! - Rich error diagnostics with position tracking
//! - WebAssembly support for browser usage
//! - Thread-safe parser instances
//! - Comprehensive test suite with golden examples
//!
//! ## API Compatibility Note
//!
//! The embedded `camxes` module differs from the standalone `camxes-rs` 0.1.x in one way:
//! - **0.1.x**: `ParseResult(cost, position, result)` - result at index 2
//! - **1.0.0+**: `ParseResult(cost, position, error_position, result)` - result at index 3
//!
//! The 1.0.0+ version adds an explicit error position field for better diagnostics.

#![allow(dead_code)]
#![allow(unused_imports)]

pub mod camxes;
pub mod cli;
pub mod eval_show;
pub mod grammar_parity;
pub mod jbo_parse;
pub mod jbo_prolog;
pub mod jbo_prop;
pub mod jbo_show;
pub mod jbo_syntax;
pub mod jbo_tree;
pub mod logic;
pub mod morphology;
pub mod parse_lojban;
pub mod parse_m;
pub mod parse_m_helpers;
pub mod bindful;
pub mod run;
pub mod util;

#[cfg(target_arch = "wasm32")]
pub mod wasm;
