//! Parser monad from [Pappy/Parse.hs](../../Pappy/Parse.hs). Single-threaded (`Rc`); packrat
//! memoization is added in generated parsers.

use super::basic::{join_errors, null_error, PResult};
use super::pos::Pos;
use std::rc::Rc;

pub type ParseResult<D, V> = PResult<D, V>;

pub trait Derivs: Clone {
    fn dv_pos(&self) -> Pos;
    fn dv_char(&self) -> ParseResult<Self, char>
    where
        Self: Sized;
}

/// `newtype Parser d v = Parser { unParser :: d -> Result d v }`
pub struct Parser<D, V> {
    run: Rc<dyn Fn(D) -> ParseResult<D, V>>,
}

impl<D, V> Clone for Parser<D, V> {
    fn clone(&self) -> Self {
        Parser {
            run: Rc::clone(&self.run),
        }
    }
}

impl<D, V> Parser<D, V> {
    pub fn new(f: impl Fn(D) -> ParseResult<D, V> + 'static) -> Self {
        Parser {
            run: Rc::new(f),
        }
    }

    pub fn parse(&self, d: D) -> ParseResult<D, V> {
        (self.run)(d)
    }
}

pub fn pure<D: Derivs + 'static, V: Clone + 'static>(x: V) -> Parser<D, V> {
    Parser::new(move |dvs: D| PResult::Parsed {
        value: x.clone(),
        rest: dvs.clone(),
        err: null_error(dvs.dv_pos()),
    })
}

pub fn bind<D: Derivs + 'static, V: 'static, W: 'static>(
    p: &Parser<D, V>,
    f: impl Fn(V) -> Parser<D, W> + Clone + 'static,
) -> Parser<D, W> {
    let p = p.clone();
    let f = f.clone();
    Parser::new(move |dvs: D| match p.parse(dvs) {
        PResult::Parsed { value, rest, err } => match f(value).parse(rest) {
            PResult::Parsed {
                value: v2,
                rest: r2,
                err: e2,
            } => PResult::Parsed {
                value: v2,
                rest: r2,
                err: join_errors(err, e2),
            },
            PResult::NoParse(e2) => PResult::NoParse(join_errors(err, e2)),
        },
        PResult::NoParse(e) => PResult::NoParse(e),
    })
}

pub fn choice<D: Derivs + 'static, V: Clone + 'static>(
    p1: Parser<D, V>,
    p2: Parser<D, V>,
) -> Parser<D, V> {
    Parser::new(move |dvs: D| match p1.parse(dvs.clone()) {
        r @ PResult::Parsed { .. } => r,
        PResult::NoParse(err1) => match p2.parse(dvs) {
            PResult::Parsed { value, rest, err } => PResult::Parsed {
                value,
                rest,
                err: join_errors(err1, err),
            },
            PResult::NoParse(err2) => PResult::NoParse(join_errors(err1, err2)),
        },
    })
}

pub fn map<D: Derivs + 'static, V: 'static, W: 'static>(
    p: Parser<D, V>,
    f: impl Fn(V) -> W + Clone + 'static,
) -> Parser<D, W> {
    let p = p.clone();
    Parser::new(move |dvs: D| match p.parse(dvs) {
        PResult::Parsed { value, rest, err } => PResult::Parsed {
            value: f(value),
            rest,
            err,
        },
        PResult::NoParse(e) => PResult::NoParse(e),
    })
}

pub fn optional<D: Derivs + 'static, V: Clone + 'static>(
    p: Parser<D, V>,
) -> Parser<D, Option<V>> {
    choice(
        map(p, Some),
        Parser::new(|dvs: D| PResult::Parsed {
            value: None,
            rest: dvs.clone(),
            err: null_error(dvs.dv_pos()),
        }),
    )
}

/// Optional `()` parse — always succeeds with `()`, advancing only if `p` matched.
pub fn opt_ignore<D: Derivs + 'static>(p: Parser<D, ()>) -> Parser<D, ()> {
    map(optional(p), |_| ())
}

/// Optional `String` parse — always succeeds; value is the parsed string or `""` if `p` did not match.
pub fn opt_string_default<D: Derivs + 'static>(p: Parser<D, String>) -> Parser<D, String> {
    map(optional(p), |o| o.unwrap_or_default())
}

/// PEG negation (`!`): succeed with `()` **without consuming** if `p` does not match at the current position.
pub fn not_followed_by<D: Derivs + 'static, V: 'static>(p: Parser<D, V>) -> Parser<D, ()> {
    Parser::new(move |dvs: D| {
        let pos = dvs.dv_pos();
        match p.parse(dvs.clone()) {
            PResult::Parsed { .. } => PResult::NoParse(null_error(pos.clone())),
            PResult::NoParse(_) => PResult::Parsed {
                value: (),
                rest: dvs,
                err: null_error(pos),
            },
        }
    })
}

/// PEG `&` (and): succeed with `()` **without consuming** if `p` **does** match at the current position.
pub fn followed_by<D: Derivs + 'static, V: 'static>(p: Parser<D, V>) -> Parser<D, ()> {
    Parser::new(move |dvs: D| match p.parse(dvs.clone()) {
        PResult::Parsed { err, .. } => PResult::Parsed {
            value: (),
            rest: dvs,
            err,
        },
        PResult::NoParse(e) => PResult::NoParse(e),
    })
}

pub fn many<D: Derivs + 'static, V: Clone + 'static>(p: Parser<D, V>) -> Parser<D, Vec<V>> {
    let p = p.clone();
    Parser::new(move |mut dvs: D| {
        let mut out = Vec::new();
        loop {
            match p.parse(dvs.clone()) {
                PResult::Parsed { value, rest, err: _ } => {
                    dvs = rest;
                    out.push(value);
                }
                PResult::NoParse(_) => {
                    let pos = dvs.dv_pos();
                    return PResult::Parsed {
                        value: out,
                        rest: dvs,
                        err: null_error(pos),
                    };
                }
            }
        }
    })
}

pub fn many1<D: Derivs + 'static, V: Clone + 'static>(p: Parser<D, V>) -> Parser<D, Vec<V>> {
    let p2 = p.clone();
    bind(&p, move |v| map(many(p2.clone()), move |mut vs| {
        vs.insert(0, v.clone());
        vs
    }))
}

pub fn any_char<D: Derivs + 'static>() -> Parser<D, char> {
    Parser::new(|dvs: D| dvs.dv_char())
}

pub fn satisfy<D: Derivs + 'static>(
    pred: impl Fn(char) -> bool + Clone + 'static,
) -> Parser<D, char> {
    bind(&any_char(), move |c| {
        if pred(c) {
            pure(c)
        } else {
            Parser::new(|dvs: D| PResult::NoParse(null_error(dvs.dv_pos())))
        }
    })
}

pub fn char_<D: Derivs + 'static>(ch: char) -> Parser<D, char> {
    satisfy(move |c| c == ch)
}

/// Parse an exact run of characters (empty string succeeds with `()` and no consumption).
pub fn string_<D: Derivs + 'static>(s: &str) -> Parser<D, ()> {
    if s.is_empty() {
        return Parser::new(|dvs: D| PResult::Parsed {
            value: (),
            rest: dvs.clone(),
            err: null_error(dvs.dv_pos()),
        });
    }
    let owned = s.to_string();
    Parser::new(move |mut dvs: D| {
        let mut acc_err = null_error(dvs.dv_pos());
        for ch in owned.chars() {
            match char_(ch).parse(dvs) {
                PResult::Parsed {
                    value: _,
                    rest,
                    err,
                } => {
                    acc_err = join_errors(acc_err, err);
                    dvs = rest;
                }
                PResult::NoParse(e) => return PResult::NoParse(join_errors(acc_err, e)),
            }
        }
        PResult::Parsed {
            value: (),
            rest: dvs,
            err: acc_err,
        }
    })
}

/// Parse an exact literal and yield it as an owned [`String`] (empty literal → `""`).
pub fn string_value<D: Derivs + 'static>(s: &str) -> Parser<D, String> {
    let owned = s.to_string();
    map(string_(s), move |_| owned.clone())
}
