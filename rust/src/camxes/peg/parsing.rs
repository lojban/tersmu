use serde::Serialize;
use std::cell::Cell;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

// Thread-local tracking the furthest byte position successfully consumed by any terminal rule
// during the current parse. This mirrors Pappy's `joinErrors` behavior: the error reported on
// parse failure is the furthest-right position the parser ever reached, not the position of
// the top-level failure. Reset before each call to `Peg::parse`.
thread_local! {
    pub static FURTHEST_POS: Cell<usize> = Cell::new(0);
    pub static FURTHEST_ERROR_POS: Cell<usize> = Cell::new(0);
    pub static ERROR_CUTOFF_POS: Cell<usize> = Cell::new(usize::MAX);
}

/// Reset the furthest-position tracker. Call before each `Peg::parse` invocation.
pub fn reset_furthest_pos() {
    FURTHEST_POS.with(|c| c.set(0));
    FURTHEST_ERROR_POS.with(|c| c.set(0));
    ERROR_CUTOFF_POS.with(|c| c.set(usize::MAX));
}

pub fn set_error_cutoff_pos(pos: usize) {
    ERROR_CUTOFF_POS.with(|c| c.set(pos));
}

fn within_error_cutoff(pos: usize) -> bool {
    ERROR_CUTOFF_POS.with(|c| pos < c.get())
}

/// Update the furthest-position tracker with a newly consumed end position.
pub fn update_furthest_pos(pos: usize) {
    if !within_error_cutoff(pos) {
        return;
    }
    FURTHEST_POS.with(|c| {
        if pos > c.get() {
            c.set(pos);
        }
    });
}

/// Set the furthest-position tracker to a specific value (used to restore after lookaheads).
pub fn set_furthest_pos(pos: usize) {
    FURTHEST_POS.with(|c| c.set(pos));
}

pub fn update_furthest_error_pos(pos: usize) {
    if !within_error_cutoff(pos) {
        return;
    }
    FURTHEST_ERROR_POS.with(|c| {
        if pos > c.get() {
            c.set(pos);
        }
    });
}

pub fn set_furthest_error_pos(pos: usize) {
    FURTHEST_ERROR_POS.with(|c| c.set(pos));
}

/// Get the furthest position reached in the current parse.
pub fn get_furthest_pos() -> usize {
    FURTHEST_POS.with(|c| c.get())
}

pub fn get_furthest_error_pos() -> usize {
    FURTHEST_ERROR_POS.with(|c| c.get())
}

pub fn limit_error_pos(pos: usize) -> usize {
    if within_error_cutoff(pos) {
        pos
    } else {
        get_furthest_error_pos()
    }
}

#[derive(Clone, Copy, Debug, Serialize)] // Add Serialize
pub struct Span(pub usize, pub usize);

#[derive(Clone, Debug, Serialize)] // Add Serialize
#[serde(tag = "type")] // Use tagged enum representation for clarity in JSON
pub enum ParseNode {
    Terminal { span: Span },
    NonTerminal {
        name: String,
        span: Span,
        children: Vec<ParseNode>,
    },
}

/// Parse result: (cost, position, error_position, payload).
/// Mirrors Pappy's semantics where both Parsed and NoParse carry an error position
/// (the furthest position where parsing failed during this parse attempt).
/// Payload is Arc-wrapped so cloning is cheap (memo cache).
#[derive(Clone)]
pub struct ParseResult(
    pub u32,                                           // cost
    pub usize,                                         // consumed position
    pub usize,                                         // error position (furthest failure)
    pub Arc<Result<Vec<ParseNode>, ParseError>>,      // payload
);

impl std::fmt::Debug for ParseResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ParseResult")
            .field(&self.0)
            .field(&self.1)
            .field(&self.2)
            .field(&self.3)
            .finish()
    }
}

#[derive(Serialize)]
struct SerializableParseResult<'a> {
    cost: u32,
    position: usize,
    error_position: usize,
    #[serde(flatten)]
    result: &'a Result<Vec<ParseNode>, ParseError>,
}

impl Serialize for ParseResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializableParseResult {
            cost: self.0,
            position: self.1,
            error_position: self.2,
            result: self.3.as_ref(),
        }
        .serialize(serializer)
    }
}


#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind")]
pub enum ErrorKind {
    UnexpectedEndOfInput,
    ExpressionDoesNotMatch,
    NotDidMatch(Vec<ParseNode>),
    NonTerminalDoesNotMatch,
    NonTerminalDoesNotExist(String),
}

/// Parse error with lazy line/column: only `position` is stored; use `line_column(input)` when needed.
#[derive(Clone, Debug, Serialize)]
pub struct ParseError {
    pub position: usize,
    /// Rule name or short description for error reporting (no full Rule clone).
    pub rule_name: String,
    pub error: ErrorKind,
    pub cause: Option<Box<ParseError>>,
}

/// Compute (1-based line, 1-based column) from input and byte position. O(position).
pub fn line_column(input: &str, position: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;
    for (i, c) in input.char_indices() {
        if i >= position {
            break;
        }
        if c == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    (line, column)
}

impl ParseError {
    /// Lazy line/column computation (call when formatting or reporting).
    pub fn line_column(&self, input: &str) -> (usize, usize) {
        line_column(input, self.position)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.cause {
            None => write!(
                f,
                "Encountered {} @ {} for '{}'",
                self.error, self.position, self.rule_name
            ),
            Some(inner) => write!(
                f,
                "Encountered {} @ {} for '{}'\n\tCaused by: {}",
                self.error, self.position, self.rule_name, inner
            ),
        }
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            ErrorKind::ExpressionDoesNotMatch => write!(f, "Expression does not match"),
            ErrorKind::NotDidMatch(nodes) => {
                write!(f, "Not predicate matched {} nodes", nodes.len())
            }
            ErrorKind::NonTerminalDoesNotMatch => write!(f, "Non-terminal does not match"),
            ErrorKind::NonTerminalDoesNotExist(name) => {
                write!(f, "Non-terminal rule '{}' does not exist", name)
            }
        }
    }
}
