//! `tersmu` library: Lojban semantic parser with integrated camxes PEG parser.

#![allow(dead_code)]
#![allow(unused_imports)]

pub mod camxes;
pub mod cli;
pub mod eval_show;
pub mod grammar_parity;
pub mod jbo_parse;
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
