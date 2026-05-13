//! A Parsing Expression Grammar (PEG) parser generator.
//!
//! # Example
//! ```rust
//! use camxes_rs::camxes::peg::grammar::Peg;
//!
//! let grammar = r#"
//! start <- 'hello' 'world'
//! "#;
//!
//! let parser = Peg::new("start", grammar).unwrap();
//! let result = parser.parse("hello world");
//! ```
//!
//! ## Semantic actions
//!
//! To build a typed AST after parsing, use [`parse_with_semantics`](crate::camxes::peg::parse_with_semantics),
//! [`ReducerTable`](crate::camxes::peg::ReducerTable), and [`SemanticNode`](crate::camxes::peg::SemanticNode). Reducers
//! run bottom-up over a successful [`ParseNode`](crate::camxes::peg::parsing::ParseNode) forest (see module
//! [`peg::semantic`](crate::camxes::peg::semantic)).
//!
//! ## Lojban grammar
//!
//! The full camxes-style Lojban PEG is embedded as [`LOJBAN_GRAMMAR`]. Build a parser with
//! `Peg::new(LOJBAN_GRAMMAR.0, LOJBAN_GRAMMAR.1)` and consume the resulting [`peg::parsing::ParseNode`]
//! forest.

pub mod peg;

/// The full Lojban PEG grammar embedded from `src/camxes/grammar/lojban.peg`.
///
/// Format: `(start_rule, grammar_text)` — the start rule is `text` (paragraph-level entry point).
pub const LOJBAN_GRAMMAR: (&str, &str) = ("text", include_str!("grammar/lojban.peg"));