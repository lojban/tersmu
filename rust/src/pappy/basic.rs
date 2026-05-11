//! Port of `Pappy.Basic` — `Result`, `ParseError`, error joining.

use super::pos::Pos;
use std::cmp::Ordering;
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ErrorDescriptor {
    Expected(String),
    Message(String),
}

#[derive(Clone, Debug, Eq)]
pub struct ParseError {
    pub error_pos: Pos,
    pub error_descrs: Vec<ErrorDescriptor>,
}

impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        self.error_pos == other.error_pos
    }
}

impl PartialOrd for ParseError {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ParseError {
    fn cmp(&self, other: &Self) -> Ordering {
        self.error_pos.cmp(&other.error_pos)
    }
}

pub fn join_errors(e1: ParseError, e2: ParseError) -> ParseError {
    let ParseError { error_pos: p, error_descrs: m } = e1;
    let ParseError {
        error_pos: p2,
        error_descrs: m2,
    } = e2;
    if p2 > p || m.is_empty() {
        ParseError {
            error_pos: p2,
            error_descrs: m2,
        }
    } else if p > p2 || m2.is_empty() {
        ParseError {
            error_pos: p,
            error_descrs: m,
        }
    } else {
        let mut seen: HashSet<ErrorDescriptor> = m.iter().cloned().collect();
        let mut merged = m;
        for y in m2 {
            if !seen.contains(&y) {
                seen.insert(y.clone());
                merged.push(y);
            }
        }
        ParseError {
            error_pos: p,
            error_descrs: merged,
        }
    }
}

pub fn msg_error(pos: Pos, msg: impl Into<String>) -> ParseError {
    ParseError {
        error_pos: pos,
        error_descrs: vec![ErrorDescriptor::Message(msg.into())],
    }
}

pub fn exp_error(pos: Pos, desc: impl Into<String>) -> ParseError {
    ParseError {
        error_pos: pos,
        error_descrs: vec![ErrorDescriptor::Expected(desc.into())],
    }
}

pub fn null_error(pos: Pos) -> ParseError {
    ParseError {
        error_pos: pos,
        error_descrs: vec![],
    }
}

/// Packrat parse result — `Parsed value rest err` in Haskell.
#[derive(Clone, Debug)]
pub enum PResult<D, V> {
    Parsed {
        value: V,
        rest: D,
        err: ParseError,
    },
    NoParse(ParseError),
}

impl<D: Clone, V: Clone> PResult<D, V> {
    pub fn map_value<U, F: FnOnce(V) -> U>(self, f: F) -> PResult<D, U> {
        match self {
            PResult::Parsed { value, rest, err } => PResult::Parsed {
                value: f(value),
                rest,
                err,
            },
            PResult::NoParse(e) => PResult::NoParse(e),
        }
    }
}

/// Fold `join_errors` like Haskell `maximum` on `[ParseError]`.
pub fn maximum_parse_errors(errors: Vec<ParseError>) -> ParseError {
    errors
        .into_iter()
        .reduce(join_errors)
        .unwrap_or_else(|| ParseError {
            error_pos: Pos {
                file: String::new(),
                line: 0,
                col: 0,
            },
            error_descrs: vec![],
        })
}

pub fn max_parse_error(a: ParseError, b: ParseError) -> ParseError {
    join_errors(a, b)
}
