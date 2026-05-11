//! Port of [`SimplifyGrammar.hs`](../../../pappy/pappy/SimplifyGrammar.hs).
//!
//! Three iterated passes until a fixpoint:
//!
//! 1. **Peephole** — local rule simplifications (flatten nested `RuleAlt`, eliminate degenerate
//!    `RuleAlt [r]`, left-factor consecutive alternatives starting with `RuleChar` into
//!    `RuleSwitchChar`, flatten nested `RuleSeq`).
//! 2. **Collapse** — eliminate nonterminals structurally equivalent to another (rename references).
//! 3. **Inline** — eliminate nonterminals referenced only once (or unreferenced), inlining their
//!    definition into call sites.
//!
//! All three passes preserve `grammar_tops` — top-level nonterminals are never eliminated.

use std::collections::HashMap;

use super::ast::{Grammar, Identifier, Match, Nonterminal, Producer, Rule};

/// Apply peephole + collapse + inline iteratively until no more simplifications are possible.
pub fn simplify_grammar(mut g: Grammar) -> Grammar {
    loop {
        let (nts1, p) = peephole(g.grammar_nonterminals);
        let (nts2, c) = collapse(&g.grammar_tops, nts1);
        let (nts3, i) = inline(&g.grammar_tops, nts2);
        g.grammar_nonterminals = nts3;
        if !p && !c && !i {
            break;
        }
    }
    g
}

// ---------------------------------------------------------------------------
// Peephole
// ---------------------------------------------------------------------------

fn peephole(nts: Vec<Nonterminal>) -> (Vec<Nonterminal>, bool) {
    let mut changed = false;
    let mut out = Vec::with_capacity(nts.len());
    for (n, t, r) in nts {
        let (r2, ch) = rule_simplify(r);
        changed |= ch;
        out.push((n, t, r2));
    }
    (out, changed)
}

fn rule_simplify(r: Rule) -> (Rule, bool) {
    match r {
        Rule::RulePos | Rule::RulePrim(_) | Rule::RuleChar(_) | Rule::RuleString(_) => (r, false),

        // Eliminate useless sequencing: `foo :: T = x:id -> id` => just `x`
        Rule::RuleSeq(ref ms, ref p) if ms.len() == 1 => {
            if let (Match::MatchName(inner, ref id), Producer::ProdName(ref id2)) = (&ms[0], p) {
                if id == id2 {
                    let (r2, _) = rule_simplify(inner.clone());
                    return (r2, true);
                }
            }
            let (ms2, sms) = seq_simplify(ms.clone());
            (Rule::RuleSeq(ms2, p.clone()), sms)
        }

        Rule::RuleSeq(ms, p) => {
            let (ms2, sms) = seq_simplify(ms);
            (Rule::RuleSeq(ms2, p), sms)
        }

        // Eliminate degenerate alternation
        Rule::RuleAlt(mut alts) if alts.len() == 1 => {
            let (r2, _) = rule_simplify(alts.remove(0));
            (r2, true)
        }

        // Flatten nested alternation
        Rule::RuleAlt(alts) if alts.iter().any(|a| matches!(a, Rule::RuleAlt(_))) => {
            let mut flat = Vec::new();
            for a in alts {
                if let Rule::RuleAlt(inner) = a {
                    flat.extend(inner);
                } else {
                    flat.push(a);
                }
            }
            let (r2, _) = rule_simplify(Rule::RuleAlt(flat));
            (r2, true)
        }

        Rule::RuleAlt(alts) => {
            let (alts2, srs) = alts_simplify(alts);
            (Rule::RuleAlt(alts2), srs)
        }

        Rule::RuleOpt(inner) => {
            let (r2, ch) = rule_simplify(*inner);
            (Rule::RuleOpt(Box::new(r2)), ch)
        }
        Rule::RuleError(inner, s) => {
            let (r2, ch) = rule_simplify(*inner);
            (Rule::RuleError(Box::new(r2), s), ch)
        }
        Rule::RuleStar(inner) => {
            let (r2, ch) = rule_simplify(*inner);
            (Rule::RuleStar(Box::new(r2)), ch)
        }
        Rule::RulePlus(inner) => {
            let (r2, ch) = rule_simplify(*inner);
            (Rule::RulePlus(Box::new(r2)), ch)
        }
        Rule::RuleExpect(inner, ss) => {
            let (r2, ch) = rule_simplify(*inner);
            (Rule::RuleExpect(Box::new(r2), ss), ch)
        }

        Rule::RuleSwitchChar(crs, dfl) => {
            let mut changed = false;
            let mut ncrs = Vec::with_capacity(crs.len());
            for (c, cr) in crs {
                let (cr2, ch) = rule_simplify(cr);
                changed |= ch;
                ncrs.push((c, cr2));
            }
            let ndfl = dfl.map(|d| {
                let (d2, ch) = rule_simplify(*d);
                changed |= ch;
                Box::new(d2)
            });
            (Rule::RuleSwitchChar(ncrs, ndfl), changed)
        }

        Rule::RuleSwitchString(id, crs, dfl) => {
            let mut changed = false;
            let mut ncrs = Vec::with_capacity(crs.len());
            for (s, cr) in crs {
                let (cr2, ch) = rule_simplify(cr);
                changed |= ch;
                ncrs.push((s, cr2));
            }
            let ndfl = dfl.map(|d| {
                let (d2, ch) = rule_simplify(*d);
                changed |= ch;
                Box::new(d2)
            });
            (Rule::RuleSwitchString(id, ncrs, ndfl), changed)
        }
    }
}

/// Left-factor consecutive alternatives starting with RuleChar into RuleSwitchChar.
fn alts_simplify(alts: Vec<Rule>) -> (Vec<Rule>, bool) {
    // Check if we have ≥2 consecutive alternatives starting with MatchAnon(RuleChar(_))
    let has_char_run = alts.windows(2).any(|w| starts_with_char(&w[0]) && starts_with_char(&w[1]));

    if has_char_run {
        return mksw(alts);
    }

    // Special degenerate case: exactly 2 alts, first starts with char, second is empty seq
    if alts.len() == 2 {
        if let (
            Rule::RuleSeq(ref ms1, ref p1),
            Rule::RuleSeq(ref ms2, ref p2),
        ) = (&alts[0], &alts[1])
        {
            if ms1.len() >= 1 && ms2.is_empty() {
                if let Match::MatchAnon(Rule::RuleChar(c)) = &ms1[0] {
                    let c = *c;
                    let rest_ms = ms1[1..].to_vec();
                    let (r1, _) = rule_simplify(Rule::RuleSeq(rest_ms, p1.clone()));
                    let (r2, _) = rule_simplify(Rule::RuleSeq(vec![], p2.clone()));
                    return (vec![Rule::RuleSwitchChar(vec![(c, r1)], Some(Box::new(r2)))], true);
                }
            }
        }
    }

    // No char factoring — just simplify individual alts
    let mut changed = false;
    let mut out = Vec::with_capacity(alts.len());
    for a in alts {
        let (a2, ch) = rule_simplify(a);
        changed |= ch;
        out.push(a2);
    }
    (out, changed)
}

fn starts_with_char(r: &Rule) -> bool {
    matches!(r, Rule::RuleSeq(ms, _) if !ms.is_empty() && matches!(&ms[0], Match::MatchAnon(Rule::RuleChar(_))))
}

/// Build a SwitchChar from consecutive character-starting alternatives.
fn mksw(alts: Vec<Rule>) -> (Vec<Rule>, bool) {
    let mut cases: Vec<(char, Rule)> = Vec::new();
    let mut remaining: Vec<Rule> = Vec::new();
    let mut collecting = true;

    for alt in alts {
        if collecting {
            if let Rule::RuleSeq(ms, p) = &alt {
                if !ms.is_empty() {
                    if let Match::MatchAnon(Rule::RuleChar(c)) = &ms[0] {
                        let c = *c;
                        let rest = Rule::RuleSeq(ms[1..].to_vec(), p.clone());
                        addcase(&mut cases, c, rest);
                        continue;
                    }
                }
                // Empty alternative → always matches, use as default, ignore rest
                if ms.is_empty() {
                    let sw = Rule::RuleSwitchChar(cases, Some(Box::new(alt)));
                    let (result, _) = alts_simplify(vec![sw]);
                    return (result, true);
                }
            }
            // Non-char alternative — stop collecting
            collecting = false;
            remaining.push(alt);
        } else {
            remaining.push(alt);
        }
    }

    let mut result = vec![Rule::RuleSwitchChar(cases, None)];
    result.extend(remaining);
    let (result2, _) = alts_simplify_inner(result);
    (result2, true)
}

fn alts_simplify_inner(alts: Vec<Rule>) -> (Vec<Rule>, bool) {
    let mut changed = false;
    let mut out = Vec::with_capacity(alts.len());
    for a in alts {
        let (a2, ch) = rule_simplify(a);
        changed |= ch;
        out.push(a2);
    }
    (out, changed)
}

/// Add a case to the case list, merging with existing case for same char.
fn addcase(cases: &mut Vec<(char, Rule)>, c: char, r: Rule) {
    for (c2, r2) in cases.iter_mut() {
        if *c2 == c {
            // Merge: make RuleAlt
            match r2 {
                Rule::RuleAlt(ref mut alts) => {
                    alts.push(r);
                }
                _ => {
                    let old = std::mem::replace(r2, Rule::RulePos); // placeholder
                    *r2 = Rule::RuleAlt(vec![old, r]);
                }
            }
            return;
        }
    }
    cases.push((c, r));
}

// Sequence simplification

fn seq_simplify(ms: Vec<Match>) -> (Vec<Match>, bool) {
    if ms.is_empty() {
        return (ms, false);
    }

    // Flatten nested sequencing: MatchAnon(RuleSeq(ms1, _)) : ms2 => ms1 ++ ms2
    if let Match::MatchAnon(Rule::RuleSeq(ref inner_ms, _)) = ms[0] {
        let mut flat = inner_ms.clone();
        flat.extend(ms[1..].to_vec());
        let (flat2, _) = seq_simplify(flat);
        return (flat2, true);
    }

    // Flatten MatchName(RuleSeq(ms1, ProdName(idi)), ido) if idi is simply bound inside ms1
    if let Match::MatchName(Rule::RuleSeq(ref inner_ms, Producer::ProdName(ref idi)), ref _ido) =
        ms[0]
    {
        if simpidi(inner_ms, idi) && nosempreds(inner_ms) {
            let rebound = rebind(inner_ms, idi, &ms[0]);
            let mut flat = rebound;
            flat.extend(ms[1..].to_vec());
            let (flat2, _) = seq_simplify(flat);
            return (flat2, true);
        }
    }

    // Flatten MatchPat(RuleSeq(ms1, ProdName(idi)), p) similarly
    if let Match::MatchPat(Rule::RuleSeq(ref inner_ms, Producer::ProdName(ref idi)), ref _p) =
        ms[0]
    {
        if simpidi(inner_ms, idi) && nosempreds(inner_ms) {
            let rebound = rebind(inner_ms, idi, &ms[0]);
            let mut flat = rebound;
            flat.extend(ms[1..].to_vec());
            let (flat2, _) = seq_simplify(flat);
            return (flat2, true);
        }
    }

    // Flatten MatchString(RuleSeq(ms1, ProdName(idi)), s) similarly
    if let Match::MatchString(Rule::RuleSeq(ref inner_ms, Producer::ProdName(ref idi)), ref _s) =
        ms[0]
    {
        if simpidi(inner_ms, idi) && nosempreds(inner_ms) {
            let rebound = rebind(inner_ms, idi, &ms[0]);
            let mut flat = rebound;
            flat.extend(ms[1..].to_vec());
            let (flat2, _) = seq_simplify(flat);
            return (flat2, true);
        }
    }

    // Recurse on head, then rest
    let (m0, ch0) = match_simplify(ms[0].clone());
    let (rest, chr) = seq_simplify(ms[1..].to_vec());
    let mut out = vec![m0];
    out.extend(rest);
    (out, ch0 || chr)
}

/// Check that `idi` is bound by a simple MatchName (not a pattern).
fn simpidi(ms: &[Match], idi: &str) -> bool {
    ms.iter().any(|m| matches!(m, Match::MatchName(_, id) if id == idi))
}

/// Check no semantic predicates.
fn nosempreds(ms: &[Match]) -> bool {
    !ms.iter().any(|m| matches!(m, Match::MatchPred(_)))
}

/// Rebind: convert the matcher for `idi` into the substitute from the enclosing match.
fn rebind(ms: &[Match], idi: &str, outer: &Match) -> Vec<Match> {
    let mut out = Vec::with_capacity(ms.len());
    for m in ms {
        if let Match::MatchName(r, id) = m {
            if id == idi {
                // Replace with the appropriate substitute
                let sub = match outer {
                    Match::MatchName(_, ido) => Match::MatchName(r.clone(), ido.clone()),
                    Match::MatchPat(_, p) => Match::MatchPat(r.clone(), p.clone()),
                    Match::MatchString(_, s) => Match::MatchString(r.clone(), s.clone()),
                    _ => m.clone(),
                };
                out.push(sub);
                continue;
            }
        }
        out.push(m.clone());
    }
    out
}

fn match_simplify(m: Match) -> (Match, bool) {
    match m {
        Match::MatchAnon(r) => {
            let (r2, ch) = rule_simplify(r);
            (Match::MatchAnon(r2), ch)
        }
        Match::MatchName(r, id) => {
            let (r2, ch) = rule_simplify(r);
            (Match::MatchName(r2, id), ch)
        }
        Match::MatchPat(r, p) => {
            let (r2, ch) = rule_simplify(r);
            (Match::MatchPat(r2, p), ch)
        }
        Match::MatchString(r, s) => {
            let (r2, ch) = rule_simplify(r);
            (Match::MatchString(r2, s), ch)
        }
        Match::MatchAnd(r) => {
            let (r2, ch) = rule_simplify(r);
            (Match::MatchAnd(r2), ch)
        }
        Match::MatchNot(r) => {
            let (r2, ch) = rule_simplify(r);
            (Match::MatchNot(r2), ch)
        }
        Match::MatchPred(_) => (m, false),
    }
}

// ---------------------------------------------------------------------------
// Collapse
// ---------------------------------------------------------------------------

/// Eliminate nonterminals structurally equivalent to some other nonterminal.
fn collapse(tops: &[Identifier], nts: Vec<Nonterminal>) -> (Vec<Nonterminal>, bool) {
    let mut rename: HashMap<String, String> = HashMap::new();

    // Scan for duplicates
    for i in 0..nts.len() {
        let (ref n, ref t, ref r) = nts[i];
        if tops.contains(n) || rename.contains_key(n) {
            continue;
        }
        // Look for a duplicate in the rest of the grammar (search from end for stability)
        for j in (0..nts.len()).rev() {
            if i == j {
                continue;
            }
            let (ref n2, ref t2, ref r2) = nts[j];
            if t2 == t {
                let rename2 = |nx: &str| -> String {
                    if nx == n { n2.clone() } else { rename.get(nx).cloned().unwrap_or_else(|| nx.to_string()) }
                };
                if equiv_rule(&rename2, r, r2) {
                    rename.insert(n.clone(), n2.clone());
                    break;
                }
            }
        }
    }

    if rename.is_empty() {
        return (nts, false);
    }

    let lookup = |nx: &str| -> String { rename.get(nx).cloned().unwrap_or_else(|| nx.to_string()) };

    let mut out = Vec::new();
    for (n, t, r) in nts {
        if rename.contains_key(&n) {
            continue;
        }
        out.push((n, t, rebuild_rule(&lookup, r)));
    }
    (out, true)
}

fn equiv_rule(f: &dyn Fn(&str) -> String, r1: &Rule, r2: &Rule) -> bool {
    match (r1, r2) {
        (Rule::RulePos, Rule::RulePos) => true,
        (Rule::RulePrim(n1), Rule::RulePrim(n2)) => f(n1) == f(n2),
        (Rule::RuleChar(c1), Rule::RuleChar(c2)) => c1 == c2,
        (Rule::RuleString(s1), Rule::RuleString(s2)) => s1 == s2,
        (Rule::RuleSeq(ms1, p1), Rule::RuleSeq(ms2, p2)) => {
            p1 == p2
                && ms1.len() == ms2.len()
                && ms1
                    .iter()
                    .zip(ms2.iter())
                    .all(|(m1, m2)| equiv_match(f, m1, m2))
        }
        (Rule::RuleAlt(rs1), Rule::RuleAlt(rs2)) => {
            rs1.len() == rs2.len()
                && rs1
                    .iter()
                    .zip(rs2.iter())
                    .all(|(a, b)| equiv_rule(f, a, b))
        }
        (Rule::RuleOpt(a), Rule::RuleOpt(b)) => equiv_rule(f, a, b),
        (Rule::RuleStar(a), Rule::RuleStar(b)) => equiv_rule(f, a, b),
        (Rule::RulePlus(a), Rule::RulePlus(b)) => equiv_rule(f, a, b),
        (Rule::RuleError(a, sa), Rule::RuleError(b, sb)) => sa == sb && equiv_rule(f, a, b),
        (Rule::RuleExpect(a, sa), Rule::RuleExpect(b, sb)) => sa == sb && equiv_rule(f, a, b),
        (Rule::RuleSwitchChar(crs1, dfl1), Rule::RuleSwitchChar(crs2, dfl2)) => {
            crs1.len() == crs2.len()
                && crs1
                    .iter()
                    .zip(crs2.iter())
                    .all(|((c1, r1), (c2, r2))| c1 == c2 && equiv_rule(f, r1, r2))
                && match (dfl1, dfl2) {
                    (Some(a), Some(b)) => equiv_rule(f, a, b),
                    (None, None) => true,
                    _ => false,
                }
        }
        (Rule::RuleSwitchString(id1, crs1, dfl1), Rule::RuleSwitchString(id2, crs2, dfl2)) => {
            id1 == id2
                && crs1.len() == crs2.len()
                && crs1
                    .iter()
                    .zip(crs2.iter())
                    .all(|((s1, r1), (s2, r2))| s1 == s2 && equiv_rule(f, r1, r2))
                && match (dfl1, dfl2) {
                    (Some(a), Some(b)) => equiv_rule(f, a, b),
                    (None, None) => true,
                    _ => false,
                }
        }
        _ => false,
    }
}

fn equiv_match(f: &dyn Fn(&str) -> String, m1: &Match, m2: &Match) -> bool {
    match (m1, m2) {
        (Match::MatchAnon(r1), Match::MatchAnon(r2)) => equiv_rule(f, r1, r2),
        (Match::MatchName(r1, id1), Match::MatchName(r2, id2)) => {
            id1 == id2 && equiv_rule(f, r1, r2)
        }
        (Match::MatchPat(r1, p1), Match::MatchPat(r2, p2)) => {
            p1 == p2 && equiv_rule(f, r1, r2)
        }
        (Match::MatchString(r1, s1), Match::MatchString(r2, s2)) => {
            s1 == s2 && equiv_rule(f, r1, r2)
        }
        (Match::MatchAnd(r1), Match::MatchAnd(r2)) => equiv_rule(f, r1, r2),
        (Match::MatchNot(r1), Match::MatchNot(r2)) => equiv_rule(f, r1, r2),
        (Match::MatchPred(p1), Match::MatchPred(p2)) => p1 == p2,
        _ => false,
    }
}

fn rebuild_rule(f: &dyn Fn(&str) -> String, r: Rule) -> Rule {
    match r {
        Rule::RulePrim(n) => Rule::RulePrim(f(&n)),
        Rule::RulePos | Rule::RuleChar(_) | Rule::RuleString(_) => r,
        Rule::RuleSeq(ms, p) => {
            Rule::RuleSeq(ms.into_iter().map(|m| rebuild_match(f, m)).collect(), p)
        }
        Rule::RuleAlt(rs) => Rule::RuleAlt(rs.into_iter().map(|r| rebuild_rule(f, r)).collect()),
        Rule::RuleOpt(inner) => Rule::RuleOpt(Box::new(rebuild_rule(f, *inner))),
        Rule::RuleError(inner, s) => Rule::RuleError(Box::new(rebuild_rule(f, *inner)), s),
        Rule::RuleStar(inner) => Rule::RuleStar(Box::new(rebuild_rule(f, *inner))),
        Rule::RulePlus(inner) => Rule::RulePlus(Box::new(rebuild_rule(f, *inner))),
        Rule::RuleExpect(inner, ss) => Rule::RuleExpect(Box::new(rebuild_rule(f, *inner)), ss),
        Rule::RuleSwitchChar(crs, dfl) => Rule::RuleSwitchChar(
            crs.into_iter()
                .map(|(c, r)| (c, rebuild_rule(f, r)))
                .collect(),
            dfl.map(|d| Box::new(rebuild_rule(f, *d))),
        ),
        Rule::RuleSwitchString(id, crs, dfl) => Rule::RuleSwitchString(
            id,
            crs.into_iter()
                .map(|(s, r)| (s, rebuild_rule(f, r)))
                .collect(),
            dfl.map(|d| Box::new(rebuild_rule(f, *d))),
        ),
    }
}

fn rebuild_match(f: &dyn Fn(&str) -> String, m: Match) -> Match {
    match m {
        Match::MatchAnon(r) => Match::MatchAnon(rebuild_rule(f, r)),
        Match::MatchName(r, id) => Match::MatchName(rebuild_rule(f, r), id),
        Match::MatchPat(r, p) => Match::MatchPat(rebuild_rule(f, r), p),
        Match::MatchString(r, s) => Match::MatchString(rebuild_rule(f, r), s),
        Match::MatchAnd(r) => Match::MatchAnd(rebuild_rule(f, r)),
        Match::MatchNot(r) => Match::MatchNot(rebuild_rule(f, r)),
        Match::MatchPred(p) => Match::MatchPred(p),
    }
}

// ---------------------------------------------------------------------------
// Inline
// ---------------------------------------------------------------------------

/// Inline nonterminals referenced 0 or 1 times (or small ones referenced few times).
fn inline(tops: &[Identifier], nts: Vec<Nonterminal>) -> (Vec<Nonterminal>, bool) {
    // Try to find one nonterminal to eliminate
    for (n, _t, r) in &nts {
        if tops.contains(n) {
            continue;
        }
        let ext_refs = refs_in_grammar(n, &nts);
        let self_refs = refs_rule(n, r);
        let elim = ext_refs == 0
            || (ext_refs == 1 && self_refs == 0)
            || (self_refs == 0 && sizeof_rule(r) <= 2);
        if elim {
            return (inline_rebuild(n, &nts), true);
        }
    }
    (nts, false)
}

/// Count references to `n` outside of `n`'s own definition.
fn refs_in_grammar(n: &str, nts: &[Nonterminal]) -> usize {
    nts.iter()
        .filter(|(n2, _, _)| n2 != n)
        .map(|(_, _, r)| refs_rule(n, r))
        .sum()
}

fn refs_rule(n: &str, r: &Rule) -> usize {
    match r {
        Rule::RulePrim(n2) => if n2 == n { 1 } else { 0 },
        Rule::RulePos | Rule::RuleChar(_) | Rule::RuleString(_) => 0,
        Rule::RuleSeq(ms, _) => ms.iter().map(|m| refs_match(n, m)).sum(),
        Rule::RuleAlt(rs) => rs.iter().map(|r| refs_rule(n, r)).sum(),
        Rule::RuleOpt(inner) | Rule::RuleError(inner, _) | Rule::RuleStar(inner) | Rule::RulePlus(inner) | Rule::RuleExpect(inner, _) => {
            refs_rule(n, inner)
        }
        Rule::RuleSwitchChar(crs, dfl) => {
            let c: usize = crs.iter().map(|(_, r)| refs_rule(n, r)).sum();
            c + dfl.as_ref().map_or(0, |d| refs_rule(n, d))
        }
        Rule::RuleSwitchString(_, crs, dfl) => {
            let c: usize = crs.iter().map(|(_, r)| refs_rule(n, r)).sum();
            c + dfl.as_ref().map_or(0, |d| refs_rule(n, d))
        }
    }
}

fn refs_match(n: &str, m: &Match) -> usize {
    match m {
        Match::MatchAnon(r)
        | Match::MatchName(r, _)
        | Match::MatchPat(r, _)
        | Match::MatchString(r, _)
        | Match::MatchAnd(r)
        | Match::MatchNot(r) => refs_rule(n, r),
        Match::MatchPred(_) => 0,
    }
}

fn sizeof_rule(r: &Rule) -> usize {
    match r {
        Rule::RulePrim(_) | Rule::RulePos | Rule::RuleChar(_) | Rule::RuleString(_) => 1,
        Rule::RuleSeq(ms, _) => 1 + ms.len(),
        Rule::RuleAlt(rs) => rs.len(),
        Rule::RuleOpt(inner) | Rule::RuleError(inner, _) | Rule::RuleStar(inner) | Rule::RulePlus(inner) | Rule::RuleExpect(inner, _) => {
            1 + sizeof_rule(inner)
        }
        Rule::RuleSwitchChar(crs, dfl) => {
            crs.len() + dfl.as_ref().map_or(0, |d| sizeof_rule(d))
        }
        Rule::RuleSwitchString(_, crs, dfl) => {
            crs.len() + dfl.as_ref().map_or(0, |d| sizeof_rule(d))
        }
    }
}

/// Rebuild the grammar, inlining all references to `target`.
fn inline_rebuild(target: &str, nts: &[Nonterminal]) -> Vec<Nonterminal> {
    // Find the definition of target
    let target_rule = nts
        .iter()
        .find(|(n, _, _)| n == target)
        .map(|(_, _, r)| r.clone())
        .expect("inline_rebuild: target not found");

    let mut out = Vec::new();
    for (n, t, r) in nts {
        if n == target {
            continue; // remove the definition
        }
        out.push((n.clone(), t.clone(), inline_rule(target, &target_rule, r.clone())));
    }
    out
}

fn inline_rule(target: &str, target_rule: &Rule, r: Rule) -> Rule {
    match r {
        Rule::RulePrim(ref n) if n == target => inline_rule(target, target_rule, target_rule.clone()),
        Rule::RulePrim(_) | Rule::RulePos | Rule::RuleChar(_) | Rule::RuleString(_) => r,
        Rule::RuleSeq(ms, p) => Rule::RuleSeq(
            ms.into_iter()
                .map(|m| inline_match(target, target_rule, m))
                .collect(),
            p,
        ),
        Rule::RuleAlt(rs) => Rule::RuleAlt(
            rs.into_iter()
                .map(|r| inline_rule(target, target_rule, r))
                .collect(),
        ),
        Rule::RuleOpt(inner) => {
            Rule::RuleOpt(Box::new(inline_rule(target, target_rule, *inner)))
        }
        Rule::RuleError(inner, s) => {
            Rule::RuleError(Box::new(inline_rule(target, target_rule, *inner)), s)
        }
        Rule::RuleStar(inner) => {
            Rule::RuleStar(Box::new(inline_rule(target, target_rule, *inner)))
        }
        Rule::RulePlus(inner) => {
            Rule::RulePlus(Box::new(inline_rule(target, target_rule, *inner)))
        }
        Rule::RuleExpect(inner, ss) => {
            Rule::RuleExpect(Box::new(inline_rule(target, target_rule, *inner)), ss)
        }
        Rule::RuleSwitchChar(crs, dfl) => Rule::RuleSwitchChar(
            crs.into_iter()
                .map(|(c, r)| (c, inline_rule(target, target_rule, r)))
                .collect(),
            dfl.map(|d| Box::new(inline_rule(target, target_rule, *d))),
        ),
        Rule::RuleSwitchString(id, crs, dfl) => Rule::RuleSwitchString(
            id,
            crs.into_iter()
                .map(|(s, r)| (s, inline_rule(target, target_rule, r)))
                .collect(),
            dfl.map(|d| Box::new(inline_rule(target, target_rule, *d))),
        ),
    }
}

fn inline_match(target: &str, target_rule: &Rule, m: Match) -> Match {
    match m {
        Match::MatchAnon(r) => Match::MatchAnon(inline_rule(target, target_rule, r)),
        Match::MatchName(r, id) => Match::MatchName(inline_rule(target, target_rule, r), id),
        Match::MatchPat(r, p) => Match::MatchPat(inline_rule(target, target_rule, r), p),
        Match::MatchString(r, s) => Match::MatchString(inline_rule(target, target_rule, r), s),
        Match::MatchAnd(r) => Match::MatchAnd(inline_rule(target, target_rule, r)),
        Match::MatchNot(r) => Match::MatchNot(inline_rule(target, target_rule, r)),
        Match::MatchPred(p) => Match::MatchPred(p),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pappyc::read_grammar::parse_grammar;
    use crate::pappyc::reduce_grammar::reduce_grammar;

    fn make_grammar(nts: Vec<Nonterminal>) -> Grammar {
        Grammar {
            grammar_name: "test".into(),
            grammar_raw_code: "".into(),
            grammar_token: None,
            grammar_imports: vec![],
            grammar_tops: vec!["main".into()],
            grammar_exports: None,
            grammar_nonterminals: nts,
        }
    }

    #[test]
    fn degenerate_alt_eliminated() {
        // main :: () = RuleAlt([RuleChar('a')])  =>  RuleChar('a')
        let nts = vec![("main".into(), "()".into(), Rule::RuleAlt(vec![Rule::RuleChar('a')]))];
        let g = simplify_grammar(make_grammar(nts));
        assert!(
            matches!(&g.grammar_nonterminals[0].2, Rule::RuleChar('a')),
            "single-alt RuleAlt should be unwrapped: {:?}",
            g.grammar_nonterminals[0].2
        );
    }

    #[test]
    fn nested_alt_flattened() {
        let nts = vec![(
            "main".into(),
            "()".into(),
            Rule::RuleAlt(vec![
                Rule::RuleAlt(vec![Rule::RuleChar('a'), Rule::RuleChar('b')]),
                Rule::RuleChar('c'),
            ]),
        )];
        let g = simplify_grammar(make_grammar(nts));
        // Should be a flat RuleSwitchChar or RuleAlt with 3 branches after peephole
        match &g.grammar_nonterminals[0].2 {
            Rule::RuleAlt(alts) => assert_eq!(alts.len(), 3, "nested RuleAlt should be flattened"),
            other => {
                // Could be RuleSwitchChar after left-factoring
                assert!(
                    matches!(other, Rule::RuleSwitchChar(_, _)),
                    "expected flattened result: {:?}",
                    other
                );
            }
        }
    }

    #[test]
    fn collapse_removes_duplicate_nts() {
        // two NTs with identical rules; one should be collapsed
        let nts = vec![
            ("main".into(), "()".into(), Rule::RulePrim("helper".into())),
            ("helper".into(), "()".into(), Rule::RuleChar('x')),
            ("dup".into(), "()".into(), Rule::RuleChar('x')),
        ];
        let g = simplify_grammar(make_grammar(nts));
        let names: Vec<_> = g.grammar_nonterminals.iter().map(|(n, _, _)| n.as_str()).collect();
        assert!(
            !names.contains(&"dup") || !names.contains(&"helper"),
            "one of helper/dup should be collapsed: {:?}",
            names
        );
    }

    #[test]
    fn inline_removes_unreferenced_nt() {
        let nts = vec![
            ("main".into(), "()".into(), Rule::RuleChar('a')),
            ("unused".into(), "()".into(), Rule::RuleChar('b')),
        ];
        let g = simplify_grammar(make_grammar(nts));
        let names: Vec<_> = g.grammar_nonterminals.iter().map(|(n, _, _)| n.as_str()).collect();
        assert!(
            !names.contains(&"unused"),
            "unreferenced NT should be inlined away: {:?}",
            names
        );
    }

    #[test]
    fn inline_single_ref_nt() {
        // main = helper, helper = 'x' — helper used once, should be inlined
        let nts = vec![
            ("main".into(), "()".into(), Rule::RulePrim("helper".into())),
            ("helper".into(), "()".into(), Rule::RuleChar('x')),
        ];
        let g = simplify_grammar(make_grammar(nts));
        let names: Vec<_> = g.grammar_nonterminals.iter().map(|(n, _, _)| n.as_str()).collect();
        assert!(!names.contains(&"helper"), "single-ref NT should be inlined: {:?}", names);
        assert!(
            matches!(&g.grammar_nonterminals[0].2, Rule::RuleChar('x')),
            "main should be inlined to RuleChar('x'): {:?}",
            g.grammar_nonterminals[0].2
        );
    }

    #[test]
    fn top_nts_never_eliminated() {
        let nts = vec![("main".into(), "()".into(), Rule::RuleChar('a'))];
        let g = simplify_grammar(make_grammar(nts));
        assert_eq!(g.grammar_nonterminals.len(), 1, "top NT must not be removed");
    }

    #[test]
    fn char_left_factoring_produces_switch_char() {
        // main = 'a' 'x' / 'a' 'y' / 'b' => RuleSwitchChar
        let nts = vec![(
            "main".into(),
            "()".into(),
            Rule::RuleAlt(vec![
                Rule::RuleSeq(
                    vec![
                        Match::MatchAnon(Rule::RuleChar('a')),
                        Match::MatchAnon(Rule::RuleChar('x')),
                    ],
                    Producer::ProdCode("()".into()),
                ),
                Rule::RuleSeq(
                    vec![
                        Match::MatchAnon(Rule::RuleChar('a')),
                        Match::MatchAnon(Rule::RuleChar('y')),
                    ],
                    Producer::ProdCode("()".into()),
                ),
                Rule::RuleSeq(
                    vec![Match::MatchAnon(Rule::RuleChar('b'))],
                    Producer::ProdCode("()".into()),
                ),
            ]),
        )];
        let g = simplify_grammar(make_grammar(nts));
        match &g.grammar_nonterminals[0].2 {
            Rule::RuleSwitchChar(cases, _dfl) => {
                assert_eq!(cases.len(), 2, "should have cases for 'a' and 'b'");
                assert_eq!(cases[0].0, 'a');
                assert_eq!(cases[1].0, 'b');
                // 'a' case should be an alt of 'x' / 'y'
                match &cases[0].1 {
                    Rule::RuleAlt(alts) => assert_eq!(alts.len(), 2),
                    Rule::RuleSwitchChar(inner, _) => assert_eq!(inner.len(), 2),
                    other => panic!("expected 'a' case to be Alt or Switch: {:?}", other),
                }
            }
            r => panic!(
                "expected RuleSwitchChar after left-factoring: {:?}",
                r
            ),
        }
    }

    #[test]
    fn simplify_morphology_smoke() {
        let src = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../Morphology.pappy"));
        let g = parse_grammar("Morphology.pappy", src).expect("parse");
        let g = reduce_grammar(g).expect("reduce");
        let g2 = simplify_grammar(g.clone());
        // After simplify we should have fewer NTs (collapse + inline eliminate duplicates)
        assert!(
            g2.grammar_nonterminals.len() <= g.grammar_nonterminals.len(),
            "simplify should not increase NT count: {} -> {}",
            g.grammar_nonterminals.len(),
            g2.grammar_nonterminals.len()
        );
    }

    #[test]
    fn simplify_lojban_smoke() {
        let src = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../Lojban.pappy"));
        let g = parse_grammar("Lojban.pappy", src).expect("parse");
        let g = reduce_grammar(g).expect("reduce");
        let g2 = simplify_grammar(g.clone());
        assert!(
            g2.grammar_nonterminals.len() <= g.grammar_nonterminals.len(),
            "simplify should not increase NT count: {} -> {}",
            g.grammar_nonterminals.len(),
            g2.grammar_nonterminals.len()
        );
    }

    #[test]
    fn useless_seq_eliminated() {
        // main :: () = x:id -> id  ≡  main :: () = x  (where x is RuleChar)
        let nts = vec![(
            "main".into(),
            "()".into(),
            Rule::RuleSeq(
                vec![Match::MatchName(Rule::RuleChar('a'), "v".into())],
                Producer::ProdName("v".into()),
            ),
        )];
        let g = simplify_grammar(make_grammar(nts));
        assert!(
            matches!(&g.grammar_nonterminals[0].2, Rule::RuleChar('a')),
            "useless seq should be eliminated: {:?}",
            g.grammar_nonterminals[0].2
        );
    }
}
