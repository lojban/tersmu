//! **Pappy** packrat runtime for Rust — ports [`Pappy.Basic`](../../Pappy/Basic.hs),
//! [`Pappy.Pos`](../../Pappy/Pos.hs), [`Pappy.Parse`](../../Pappy/Parse.hs).
//!
//! # Grammar source of truth
//!
//! Grammars are **`*.pappy`** (from `*.pappy.rhs` + [`scripts/gen_pappy.py`](../../scripts/gen_pappy.py)).
//! This crate does **not** use Pest; do not add PEG grammars here.
//!
//! # Roadmap
//!
//! 1. Extend [`parser`](parser) as needed (mirrors `Parse.hs`).
//! 2. Implement Rust parser emission in [`pappy/WriteParserRust.hs`](../../pappy/pappy/WriteParserRust.hs)
//!    from the same IR as `WriteParser.hs`.
//! 3. Alternatives like LALRPOP need a **new** grammar backend; Pappy’s packrat + left recursion maps
//!    naturally to this runtime.

#![allow(unused_imports)]

pub mod basic;
pub mod parser;
pub mod pos;

pub use basic::{
    join_errors, max_parse_error, maximum_parse_errors, msg_error, null_error, ErrorDescriptor,
    ParseError, PResult,
};
pub use pos::{next_pos, Pos};
