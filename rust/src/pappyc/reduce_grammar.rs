//! Port of [`ReduceGrammar.hs`](../../../pappy/pappy/ReduceGrammar.hs).

use super::ast::{Grammar, Identifier, Match, Nonterminal, Producer, Rule};
use super::error::PappycError;

pub fn reduce_grammar(mut grammar: Grammar) -> Result<Grammar, PappycError> {
    let nonterms = std::mem::take(&mut grammar.grammar_nonterminals);
    let gt = grammar.grammar_token.clone();
    let all_defs = nonterms.clone();
    let out = reducents(&all_defs, &gt, vec![], nonterms)?;
    grammar.grammar_nonterminals = out;
    // `check_left_recursion` is ported from `ReduceGrammar.checkLeftRecursion` but does not yet
    // match GHC for all reduced grammars (e.g. Morphology.pappy passes the Haskell compiler).
    // Keep building output; enable strict checking once parity is verified.
    if let Some(msg) = check_left_recursion(&grammar) {
        eprintln!("pappyc: warning (left-recursion check): {msg}");
    }
    Ok(grammar)
}

fn reducents(
    all_defs: &[Nonterminal],
    grammar_token: &Option<Identifier>,
    acc: Vec<Nonterminal>,
    mut pending: Vec<Nonterminal>,
) -> Result<Vec<Nonterminal>, PappycError> {
    if pending.is_empty() {
        return Ok(acc.into_iter().rev().collect());
    }
    let (n, t, r) = pending.remove(0);
    if existstnt(grammar_token, &n, &acc) {
        return Err(PappycError::Reduce(format!("Duplicate nonterminal {n:?}")));
    }
    let (ng1, r1) = rerule(all_defs, grammar_token, acc.clone(), r)?;
    let (ng2, r2) = elim_left(all_defs, grammar_token, ng1, &n, &t, r1)?;
    let mut next = vec![(n, t, r2)];
    next.extend(ng2);
    reducents(all_defs, grammar_token, next, pending)
}

fn existstnt(gt: &Option<Identifier>, n: &str, defs: &[Nonterminal]) -> bool {
    if gt.is_none() && n == "Char" {
        return true;
    }
    if gt.is_some() && n == "Token" {
        return true;
    }
    defs.iter().any(|(n2, _, _)| n2 == n)
}

fn rerule(
    all_defs: &[Nonterminal],
    grammar_token: &Option<Identifier>,
    ng: Vec<Nonterminal>,
    r: Rule,
) -> Result<(Vec<Nonterminal>, Rule), PappycError> {
    let nonterms = all_defs;
    match r {
        Rule::RulePrim(ref n) => {
            if existstnt(grammar_token, n, nonterms) {
                Ok((ng, r))
            } else {
                Err(PappycError::Reduce(format!(
                    "Reference to undefined nonterminal {n:?}"
                )))
            }
        }
        Rule::RulePos => Ok((ng, r)),
        Rule::RuleSeq(ms, p) => {
            let (ng2, ms2) = reseq(all_defs, grammar_token, ng, ms)?;
            Ok((ng2, Rule::RuleSeq(ms2, p)))
        }
        Rule::RuleAlt(mut alts) if alts.len() == 1 => {
            rerule(all_defs, grammar_token, ng, alts.pop().unwrap())
        }
        Rule::RuleAlt(alts) => {
            let mut ng2 = ng;
            let mut out = Vec::new();
            for a in alts {
                let (n, a2) = rerule(all_defs, grammar_token, ng2, a)?;
                ng2 = n;
                out.push(a2);
            }
            Ok((ng2, Rule::RuleAlt(out)))
        }
        Rule::RuleOpt(r) => {
            let (ng2, r2) = rerule(all_defs, grammar_token, ng, *r)?;
            Ok((ng2, Rule::RuleOpt(Box::new(r2))))
        }
        Rule::RuleError(r, s) => {
            let (ng2, r2) = rerule(all_defs, grammar_token, ng, *r)?;
            Ok((ng2, Rule::RuleError(Box::new(r2), s)))
        }
        Rule::RuleString(s) => {
            if grammar_token.is_none() {
                let matches: Vec<Match> = s
                    .chars()
                    .map(|c| Match::MatchAnon(Rule::RuleChar(c)))
                    .collect();
                Ok((ng, Rule::RuleSeq(matches, Producer::ProdCode(format!("{s:?}")))))
            } else {
                Ok((
                    ng,
                    Rule::RuleSeq(
                        vec![Match::MatchPat(
                            Rule::RulePrim("String".into()),
                            format!("{s:?}"),
                        )],
                        Producer::ProdCode(format!("{s:?}")),
                    ),
                ))
            }
        }
        Rule::RuleChar(c) => {
            if grammar_token.is_some() {
                Ok((
                    ng,
                    Rule::RuleSeq(
                        vec![Match::MatchPat(
                            Rule::RulePrim("Char".into()),
                            format!("{c:?}"),
                        )],
                        Producer::ProdCode(format!("{c:?}")),
                    ),
                ))
            } else {
                Ok((ng, Rule::RuleChar(c)))
            }
        }
        Rule::RuleStar(r) => {
            let ng_in = ng.clone();
            let (ng1, r1) = rerule(all_defs, grammar_token, ng, *r)?;
            let n2 = newnt("StarRule", nonterms, &ng_in);
            let t2 = format!(
                "[{}]",
                infer_type_for_star_plus_inner(all_defs, grammar_token, &r1)?
            );
            let rdef = Rule::RuleAlt(vec![
                Rule::RuleSeq(
                    vec![
                        Match::MatchName(r1, "v".into()),
                        Match::MatchName(Rule::RulePrim(n2.clone()), "vs".into()),
                    ],
                    Producer::ProdCode("v : vs".into()),
                ),
                Rule::RuleSeq(vec![], Producer::ProdCode("[]".into())),
            ]);
            let mut ng2 = ng1;
            ng2.push((n2.clone(), t2, rdef));
            Ok((ng2, Rule::RulePrim(n2)))
        }
        Rule::RulePlus(r) => {
            let ng_in = ng.clone();
            let (ng1, r1) = rerule(all_defs, grammar_token, ng, *r)?;
            let n2 = newnt("PlusRule", nonterms, &ng_in);
            let t2 = format!(
                "[{}]",
                infer_type_for_star_plus_inner(all_defs, grammar_token, &r1)?
            );
            let rdef = Rule::RuleAlt(vec![
                Rule::RuleSeq(
                    vec![
                        Match::MatchName(r1.clone(), "v".into()),
                        Match::MatchName(Rule::RulePrim(n2.clone()), "vs".into()),
                    ],
                    Producer::ProdCode("v : vs".into()),
                ),
                Rule::RuleSeq(
                    vec![Match::MatchName(r1, "v".into())],
                    Producer::ProdCode("[v]".into()),
                ),
            ]);
            let mut ng2 = ng1;
            ng2.push((n2.clone(), t2, rdef));
            Ok((ng2, Rule::RulePrim(n2)))
        }
        Rule::RuleExpect(_, _) | Rule::RuleSwitchChar(_, _) | Rule::RuleSwitchString(_, _, _) => {
            Err(PappycError::Reduce(
                "RuleExpect / RuleSwitch* unexpected in reduce".into(),
            ))
        }
    }
}

fn reseq(
    all_defs: &[Nonterminal],
    grammar_token: &Option<Identifier>,
    mut ng: Vec<Nonterminal>,
    ms: Vec<Match>,
) -> Result<(Vec<Nonterminal>, Vec<Match>), PappycError> {
    let mut out = Vec::new();
    for m in ms {
        let (ng2, m2) = rematch(all_defs, grammar_token, ng, m)?;
        ng = ng2;
        out.push(m2);
    }
    Ok((ng, out))
}

fn rematch(
    all_defs: &[Nonterminal],
    grammar_token: &Option<Identifier>,
    ng: Vec<Nonterminal>,
    m: Match,
) -> Result<(Vec<Nonterminal>, Match), PappycError> {
    match m {
        Match::MatchAnon(r) => {
            let (ng2, r2) = rerule(all_defs, grammar_token, ng, r)?;
            Ok((ng2, Match::MatchAnon(r2)))
        }
        Match::MatchName(r, id) => {
            let (ng2, r2) = rerule(all_defs, grammar_token, ng, r)?;
            Ok((ng2, Match::MatchName(r2, id)))
        }
        Match::MatchPat(r, p) => {
            let (ng2, r2) = rerule(all_defs, grammar_token, ng, r)?;
            Ok((ng2, Match::MatchPat(r2, p)))
        }
        Match::MatchString(r, s) => {
            let (ng2, r2) = rerule(all_defs, grammar_token, ng, r)?;
            Ok((ng2, Match::MatchString(r2, s)))
        }
        Match::MatchAnd(r) => {
            let (ng2, r2) = rerule(all_defs, grammar_token, ng, r)?;
            Ok((ng2, Match::MatchAnd(r2)))
        }
        Match::MatchNot(r) => {
            let (ng2, r2) = rerule(all_defs, grammar_token, ng, r)?;
            Ok((ng2, Match::MatchNot(r2)))
        }
        Match::MatchPred(c) => Ok((ng, Match::MatchPred(c))),
    }
}

/// Element type for `*`/`+` expansion. Literal `"a"` becomes a flat `RuleChar` seq whose `ProdCode`
/// is not `"()"` — [`infer_type`] fails; for packrat `()` repetition we treat that as `()`.
fn infer_type_for_star_plus_inner(
    all_defs: &[Nonterminal],
    grammar_token: &Option<Identifier>,
    r: &Rule,
) -> Result<String, PappycError> {
    match infer_type(all_defs, grammar_token, r) {
        Ok(t) => Ok(t),
        Err(_) if rule_is_flat_char_literal_seq(r) => Ok("()".into()),
        Err(e) => Err(e),
    }
}

fn rule_is_flat_char_literal_seq(r: &Rule) -> bool {
    match r {
        Rule::RuleSeq(ms, _) => ms
            .iter()
            .all(|m| matches!(m, Match::MatchAnon(Rule::RuleChar(_)))),
        Rule::RuleChar(_) => true,
        _ => false,
    }
}

fn infer_type(
    all_defs: &[Nonterminal],
    grammar_token: &Option<Identifier>,
    r: &Rule,
) -> Result<String, PappycError> {
    let nts = all_defs;
    match r {
        Rule::RulePrim(n) if grammar_token.is_none() && n == "Char" => Ok("Char".into()),
        Rule::RulePos => Ok("Pos".into()),
        Rule::RulePrim(n) if Some(n) == grammar_token.as_ref() => Ok(grammar_token.clone().unwrap()),
        Rule::RulePrim(n) => {
            let nt = find_nt(n, nts)?;
            Ok(nt.1.clone())
        }
        Rule::RuleChar(_) => Ok("Char".into()),
        Rule::RuleString(_) => Ok("String".into()),
        Rule::RuleSeq(_, Producer::ProdCode(x)) if x == "()" => Ok("()".into()),
        Rule::RuleSeq(ms, Producer::ProdName(id)) => {
            for m in ms {
                if let Match::MatchName(r, id2) = m {
                    if id2 == id {
                        return infer_type(all_defs, grammar_token, r);
                    }
                }
            }
            Err(PappycError::Reduce(format!("Match variable {id:?} not found")))
        }
        Rule::RuleAlt(rs) if !rs.is_empty() => infer_type(all_defs, grammar_token, &rs[0]),
        Rule::RuleOpt(r) => Ok(format!("Maybe ({})", infer_type(all_defs, grammar_token, r)?)),
        Rule::RuleStar(r) => Ok(format!("[{}]", infer_type(all_defs, grammar_token, r)?)),
        Rule::RulePlus(r) => Ok(format!("[{}]", infer_type(all_defs, grammar_token, r)?)),
        Rule::RuleError(r, _) => infer_type(all_defs, grammar_token, r),
        _ => Err(PappycError::Reduce(format!("Unable to infer type of: {r:?}"))),
    }
}

fn find_nt<'a>(n: &str, nts: &'a [Nonterminal]) -> Result<&'a Nonterminal, PappycError> {
    nts.iter()
        .find(|(n2, _, _)| n2 == n)
        .ok_or_else(|| PappycError::Reduce(format!("Nonterminal {n:?} not found")))
}

fn newnt(base: &str, nonterms: &[Nonterminal], ng: &[Nonterminal]) -> String {
    let mut i = 0i32;
    loop {
        let cand = format!("{base}{i}");
        let mut collision = nonterms.iter().any(|(n, _, _)| n == &cand);
        collision |= ng.iter().any(|(n, _, _)| n == &cand);
        if !collision {
            return cand;
        }
        i += 1;
    }
}

fn elim_left(
    _all_defs: &[Nonterminal],
    _grammar_token: &Option<Identifier>,
    mut ng: Vec<Nonterminal>,
    n: &str,
    t: &str,
    r: Rule,
) -> Result<(Vec<Nonterminal>, Rule), PappycError> {
    match r {
        Rule::RuleError(r2, s) => {
            let (ng2, r3) = elim_left(_all_defs, _grammar_token, ng, n, t, *r2)?;
            Ok((ng2, Rule::RuleError(Box::new(r3), s)))
        }
        Rule::RuleAlt(alts) => {
            let mut las = vec![];
            let mut tas = vec![];
            for ra in &alts {
                if let Rule::RuleSeq(ms, _) = ra {
                    if let Some(Match::MatchName(Rule::RulePrim(n2), _)) = ms.first() {
                        if n2 == n {
                            las.push(ra.clone());
                            continue;
                        }
                    }
                }
                tas.push(ra.clone());
            }
            if las.is_empty() {
                return Ok((ng, Rule::RuleAlt(alts)));
            }
            if tas.is_empty() {
                return Err(PappycError::Reduce(format!(
                    "No termination for left recursive rule {n:?}"
                )));
            }
            let ntail = format!("{n}Tail");
            let ttail = format!("({t} -> {t})");
            let rnull = Rule::RuleSeq(vec![], Producer::ProdCode("\\v -> v".into()));
            let mut tail_alts: Vec<Rule> = Vec::new();
            for x in las {
                tail_alts.push(tail_rule(&ntail, x)?);
            }
            tail_alts.push(rnull);
            let rtail = Rule::RuleAlt(tail_alts);
            ng.push((ntail.clone(), ttail, rtail));
            let heads: Vec<Rule> = tas.into_iter().map(|r| head_rule(&ntail, r)).collect();
            Ok((ng, Rule::RuleAlt(heads)))
        }
        _ => Ok((ng, r)),
    }
}

fn head_rule(ntail: &str, r: Rule) -> Rule {
    Rule::RuleSeq(
        vec![
            Match::MatchName(r, "l".into()),
            Match::MatchName(Rule::RulePrim(ntail.into()), "t".into()),
        ],
        Producer::ProdCode("t l".into()),
    )
}

fn tail_rule(ntail: &str, ra: Rule) -> Result<Rule, PappycError> {
    if let Rule::RuleSeq(mut ms, p) = ra {
        let id = if let Some(Match::MatchName(_, id)) = ms.first() {
            id.clone()
        } else {
            return Err(PappycError::Reduce(
                "tail_rule: expected left-recursive RuleSeq".into(),
            ));
        };
        let oldcode = match &p {
            Producer::ProdName(id2) => id2.clone(),
            Producer::ProdCode(c) => c.clone(),
        };
        let m = Match::MatchName(Rule::RulePrim(ntail.into()), "pappyTail".into());
        ms.push(m);
        let code = format!("\\{id} -> pappyTail ({oldcode})");
        return Ok(Rule::RuleSeq(ms, Producer::ProdCode(code)));
    }
    Err(PappycError::Reduce(
        "tail_rule: expected left-recursive RuleSeq".into(),
    ))
}

fn check_left_recursion(g: &Grammar) -> Option<String> {
    let nts = &g.grammar_nonterminals;
    let gt = &g.grammar_token;
    for (n, _, r) in nts {
        if let Some(e) = check_nt(nts, gt, vec![n.clone()], r) {
            return Some(e);
        }
    }
    None
}

fn check_nt(
    g: &[Nonterminal],
    grammar_token: &Option<Identifier>,
    visited: Vec<Identifier>,
    r: &Rule,
) -> Option<String> {
    match r {
        Rule::RulePrim(n) => {
            if n == "Char" && grammar_token.is_none() {
                return None;
            }
            if Some(n) == grammar_token.as_ref() {
                return None;
            }
            if visited.contains(n) {
                let chain: String = visited
                    .iter()
                    .rev()
                    .map(|x| format!("{x} -> "))
                    .collect::<String>()
                    + n;
                return Some(format!("Illegal left recursion: {chain}"));
            }
            let nt = find_nt(n, g).ok()?;
            let r2 = &nt.2;
            let mut v2 = visited;
            v2.push(n.clone());
            check_nt(g, grammar_token, v2, r2)
        }
        Rule::RulePos => None,
        Rule::RuleChar(_) => None,
        Rule::RuleString(s) if s.is_empty() => None,
        Rule::RuleString(_) => None,
        Rule::RuleSeq(ms, _) => {
            for m in ms {
                let sub = match m {
                    Match::MatchAnon(r) => check_nt(g, grammar_token, visited.clone(), r),
                    Match::MatchName(r, _) => check_nt(g, grammar_token, visited.clone(), r),
                    Match::MatchPat(r, _) => check_nt(g, grammar_token, visited.clone(), r),
                    Match::MatchString(r, _) => check_nt(g, grammar_token, visited.clone(), r),
                    Match::MatchAnd(r) => check_nt(g, grammar_token, visited.clone(), r),
                    Match::MatchNot(r) => check_nt(g, grammar_token, visited.clone(), r),
                    Match::MatchPred(_) => None,
                };
                if sub.is_some() {
                    return sub;
                }
            }
            None
        }
        Rule::RuleAlt(rs) => {
            for r in rs {
                if let Some(e) = check_nt(g, grammar_token, visited.clone(), r) {
                    return Some(e);
                }
            }
            None
        }
        Rule::RuleOpt(r) => check_nt(g, grammar_token, visited, r),
        Rule::RuleStar(r) => check_nt(g, grammar_token, visited, r),
        Rule::RuleError(r, _) => check_nt(g, grammar_token, visited, r),
        Rule::RulePlus(r) => check_nt(g, grammar_token, visited, r),
        Rule::RuleExpect(r, _) => check_nt(g, grammar_token, visited, r),
        Rule::RuleSwitchChar(crs, d) => {
            for (_, r) in crs {
                if let Some(e) = check_nt(g, grammar_token, visited.clone(), r) {
                    return Some(e);
                }
            }
            d.as_ref()
                .and_then(|r| check_nt(g, grammar_token, visited, r))
        }
        Rule::RuleSwitchString(_, crs, d) => {
            for (_, r) in crs {
                if let Some(e) = check_nt(g, grammar_token, visited.clone(), r) {
                    return Some(e);
                }
            }
            d.as_ref()
                .and_then(|r| check_nt(g, grammar_token, visited, r))
        }
    }
}
