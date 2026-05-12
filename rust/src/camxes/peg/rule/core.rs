use super::types::Rule;
use crate::camxes::peg::grammar::{MemoMap, Peg};
use crate::camxes::peg::parsing::{get_furthest_error_pos, get_furthest_pos, limit_error_pos, set_furthest_error_pos, set_furthest_pos, update_furthest_error_pos, update_furthest_pos, ErrorKind, ParseError, ParseNode, ParseResult, Span};
use log::{debug, log_enabled, Level};
use std::sync::Arc;

/// Helper: merge two error positions, keeping the furthest (Pappy's joinErrors).
fn join_error_pos(pos1: usize, pos2: usize) -> usize {
    pos1.max(limit_error_pos(pos2))
}

impl Rule {
    pub fn parse(&self, peg: &Peg, input: &str, position: usize, depth: usize, memo: &mut MemoMap) -> ParseResult {
        match self {
            Rule::Empty => ParseResult(1, position, position, Arc::new(Ok(vec![]))),

            Rule::Any => {
                if position < input.len() {
                    let new_pos = position + 1;
                    update_furthest_pos(new_pos);
                    ParseResult(
                        1,
                        new_pos,
                        position, // error_pos: no error, use current position
                        Arc::new(Ok(vec![ParseNode::Terminal {
                            span: Span(position, new_pos),
                        }])),
                    )
                } else {
                    update_furthest_error_pos(position);
                    ParseResult(
                        1,
                        position,
                        limit_error_pos(position), // error_pos: failed at current position
                        Arc::new(Err(ParseError {
                            position,
                            rule_name: self.error_description(),
                            error: ErrorKind::UnexpectedEndOfInput,
                            cause: None,
                        })),
                    )
                }
            }

            Rule::Literal(pattern) => {
                // Fast path: single-byte literal
                let matched = if pattern.len() == 1 {
                    position < input.len() && input.as_bytes()[position] == pattern.as_bytes()[0]
                } else {
                    input[position..].starts_with(pattern)
                };
                if matched {
                    let len = pattern.len();
                    let new_pos = position + len;
                    update_furthest_pos(new_pos);
                    ParseResult(
                        1,
                        new_pos,
                        position, // error_pos: no error
                        Arc::new(Ok(vec![ParseNode::Terminal {
                            span: Span(position, new_pos),
                        }])),
                    )
                } else {
                    update_furthest_error_pos(position);
                    ParseResult(
                        1,
                        position,
                        limit_error_pos(position), // error_pos: failed at current position
                        Arc::new(Err(ParseError {
                            position,
                            rule_name: self.error_description(),
                            error: ErrorKind::ExpressionDoesNotMatch,
                            cause: None,
                        })),
                    )
                }
            }

            Rule::NonTerminal(name) => {
                let key = (name.clone(), position);

                if let Some(cached_result) = memo.get(&key) {
                    if log_enabled!(Level::Debug) {
                        debug!(
                            "{}cache hit {name} @ {position} -> {}",
                            "│".repeat(depth),
                            cached_result.1
                        );
                    }
                    return cached_result.clone();
                }

                if log_enabled!(Level::Debug) {
                    debug!("{}parsing {name} @ {position}", "│".repeat(depth));
                }

                let rule = match peg.rules.get(name) {
                    Some(r) => r,
                    None => {
                        let err = ParseError {
                            position,
                            rule_name: self.error_description(),
                            error: ErrorKind::NonTerminalDoesNotExist(name.clone()),
                            cause: None,
                        };
                        let res = ParseResult(1, position, position, Arc::new(Err(err)));
                        memo.insert(key, res.clone());
                        return res;
                    }
                };

                let result = match rule.parse(peg, input, position, depth + 1, memo) {
                    ParseResult(cost, new_pos, err_pos, ref payload) => match payload.as_ref() {
                        Ok(matches) => ParseResult(
                            cost,
                            new_pos,
                            err_pos, // propagate error position from inner parse
                            Arc::new(Ok(vec![ParseNode::NonTerminal {
                                name: name.clone(),
                                span: Span(position, new_pos),
                                children: matches.clone(),
                            }])),
                        ),
                        Err(inner) => ParseResult(
                            cost,
                            position,
                            err_pos, // propagate error position from inner parse
                            Arc::new(Err(ParseError {
                                position: limit_error_pos(position),
                                rule_name: self.error_description(),
                                error: ErrorKind::NonTerminalDoesNotMatch,
                                cause: Some(Box::new(inner.clone())),
                            })),
                        ),
                    },
                };

                if log_enabled!(Level::Debug) {
                    debug!(
                        "{}└{} {} @ {} -> {}",
                        "│".repeat(depth),
                        if result.3.is_ok() { "ok" } else { "err" },
                        name,
                        position,
                        result.1
                    );
                }
                memo.insert(key, result.clone());
                result
            }

            Rule::Choice(choices) => {
                let mut furthest_err = position;
                for choice in choices {
                    let res = choice.parse(peg, input, position, depth, memo);
                    furthest_err = join_error_pos(furthest_err, res.2);
                    if res.3.is_ok() {
                        // Success: return with merged error position
                        return ParseResult(1, res.1, furthest_err, res.3);
                    }
                }
                // All choices failed: return error with furthest position
                ParseResult(
                    1,
                    position,
                    furthest_err,
                    Arc::new(Err(ParseError {
                        position: furthest_err,
                        rule_name: self.error_description(),
                        error: ErrorKind::ExpressionDoesNotMatch,
                        cause: None,
                    })),
                )
            }

            Rule::Sequence(sequence) => {
                let mut captures = Vec::with_capacity(sequence.len());
                let mut pos = position;
                let mut furthest_err = position;

                for expr in sequence {
                    let res = expr.parse(peg, input, pos, depth, memo);
                    furthest_err = join_error_pos(furthest_err, res.2);
                    match res.3.as_ref() {
                        Ok(m) => {
                            pos = res.1;
                            captures.extend(m.iter().cloned());
                        }
                        Err(e) => {
                            return ParseResult(1, position, furthest_err, Arc::new(Err(e.clone())));
                        }
                    }
                }
                ParseResult(1, pos, furthest_err, Arc::new(Ok(captures)))
            }

            Rule::ZeroOrMore(expr) => {
                let mut captures = Vec::with_capacity(8);
                let mut pos = position;
                let mut furthest_err = position;

                loop {
                    let res = expr.parse(peg, input, pos, depth, memo);
                    furthest_err = join_error_pos(furthest_err, res.2);
                    match res.3.as_ref() {
                        Ok(m) => {
                            // Prevent infinite loop on zero-width matches
                            if res.1 == pos {
                                break;
                            }
                            pos = res.1;
                            captures.extend(m.iter().cloned());
                        }
                        Err(_) => break,
                    }
                }
                ParseResult(1, pos, furthest_err, Arc::new(Ok(captures)))
            }

            Rule::OneOrMore(expr) => Rule::Sequence(vec![
                Rule::Group(expr.clone()),
                Rule::ZeroOrMore(expr.clone()),
            ])
            .parse(peg, input, position, depth, memo),

            Rule::Optional(expr) => {
                let res = expr.parse(peg, input, position, depth, memo);
                if res.3.is_ok() {
                    ParseResult(1, res.1, res.2, res.3)
                } else {
                    // Optional always succeeds; propagate error position from failed attempt
                    ParseResult(1, position, res.2, Arc::new(Ok(vec![])))
                }
            }

            Rule::And(expr) => {
                // Lookahead &(e): succeeds without consuming input.
                // Lookaheads are speculative - error positions from inside should not propagate.
                // Save and restore both FURTHEST_POS and the error_position.
                let saved_furthest = get_furthest_pos();
                let saved_furthest_error = get_furthest_error_pos();
                let res = expr.parse(peg, input, position, depth, memo);
                set_furthest_pos(saved_furthest);
                set_furthest_error_pos(saved_furthest_error);
                // Use saved_furthest as error_pos (don't propagate inner error_pos)
                if res.3.is_ok() {
                    ParseResult(1, position, saved_furthest, Arc::new(Ok(vec![])))
                } else {
                    let e = res.3.as_ref().clone().err().unwrap();
                    ParseResult(1, position, saved_furthest, Arc::new(Err(e)))
                }
            }

            Rule::Not(expr) => {
                // Lookahead !(e): succeeds without consuming input when inner fails.
                // Lookaheads are speculative - error positions from inside should not propagate.
                // Save and restore both FURTHEST_POS and the error_position.
                let saved_furthest = get_furthest_pos();
                let saved_furthest_error = get_furthest_error_pos();
                let res = expr.parse(peg, input, position, depth, memo);
                set_furthest_pos(saved_furthest);
                set_furthest_error_pos(saved_furthest_error);
                // Use saved_furthest as error_pos (don't propagate inner error_pos)
                match res.3.as_ref() {
                    Ok(m) => {
                        // Inner succeeded → Not fails
                        ParseResult(
                            1,
                            position,
                            saved_furthest,
                            Arc::new(Err(ParseError {
                                position: limit_error_pos(position),
                                rule_name: self.error_description(),
                                error: ErrorKind::NotDidMatch(m.clone()),
                                cause: None,
                            })),
                        )
                    }
                    Err(_) => {
                        // Inner failed → Not succeeds
                        ParseResult(1, position, saved_furthest, Arc::new(Ok(vec![])))
                    }
                }
            }

            Rule::Group(expr) => expr.parse(peg, input, position, depth, memo),

            Rule::Range(start, end) => {
                if position < input.len() {
                    let c = input[position..].chars().next().unwrap();
                    if start.chars().next().unwrap() <= c && c <= end.chars().next().unwrap() {
                        let new_pos = position + 1;
                        update_furthest_pos(new_pos);
                        ParseResult(
                            1,
                            new_pos,
                            position, // no error
                            Arc::new(Ok(vec![ParseNode::Terminal {
                                span: Span(position, new_pos),
                            }])),
                        )
                    } else {
                        update_furthest_error_pos(position);
                        ParseResult(
                            1,
                            position,
                            limit_error_pos(position), // error at current position
                            Arc::new(Err(ParseError {
                                position: limit_error_pos(position),
                                rule_name: self.error_description(),
                                error: ErrorKind::ExpressionDoesNotMatch,
                                cause: None,
                            })),
                        )
                    }
                } else {
                    update_furthest_error_pos(position);
                    ParseResult(
                        1,
                        position,
                        limit_error_pos(position), // error at current position
                        Arc::new(Err(ParseError {
                            position: limit_error_pos(position),
                            rule_name: self.error_description(),
                            error: ErrorKind::ExpressionDoesNotMatch,
                            cause: None,
                        })),
                    )
                }
            }

            Rule::Class(symbols) => {
                // Longest match first: sort by length descending
                let mut syms: Vec<&String> = symbols.iter().collect();
                syms.sort_by_key(|b| std::cmp::Reverse(b.len()));
                let matched = syms
                    .into_iter()
                    .find(|s| input[position..].starts_with(s.as_str()));
                if let Some(symbol) = matched {
                    let len = symbol.len();
                    let new_pos = position + len;
                    update_furthest_pos(new_pos);
                    ParseResult(
                        1,
                        new_pos,
                        position, // no error
                        Arc::new(Ok(vec![ParseNode::Terminal {
                            span: Span(position, new_pos),
                        }])),
                    )
                } else {
                    update_furthest_error_pos(position);
                    ParseResult(
                        1,
                        position,
                        limit_error_pos(position), // error at current position
                        Arc::new(Err(ParseError {
                            position: limit_error_pos(position),
                            rule_name: self.error_description(),
                            error: ErrorKind::ExpressionDoesNotMatch,
                            cause: None,
                        })),
                    )
                }
            }
        }
    }
}
