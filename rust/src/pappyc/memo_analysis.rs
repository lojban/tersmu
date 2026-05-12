//! Port of [`MemoAnalysis.hs`](../../../pappy/pappy/MemoAnalysis.hs).

use super::ast::{Identifier, Match, Nonterminal, Rule};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Cost {
    Finite(i32),
    Infinite,
}

impl Cost {
    fn add(self, o: Cost) -> Cost {
        match (self, o) {
            (Cost::Finite(a), Cost::Finite(b)) => Cost::Finite(a + b),
            _ => Cost::Infinite,
        }
    }
}

pub fn memo_analysis(nts: &[Nonterminal]) -> Vec<Identifier> {
    fn iter(vnts: Vec<Nonterminal>, all: &[Nonterminal]) -> Vec<Identifier> {
        match sel(&vnts, None, all) {
            Some(vnt) => {
                let mut next = vnts;
                next.insert(0, vnt);
                iter(next, all)
            }
            None => memos(all, &vnts),
        }
    }
    iter(vec![], nts)
}

fn memos(nts: &[Nonterminal], vnts: &[Nonterminal]) -> Vec<Identifier> {
    let mut out = Vec::new();
    for (n, _, _) in nts {
        if find_nt(n, vnts).is_some() {
            continue;
        }
        out.push(n.clone());
    }
    out
}

fn sel(
    vnts: &[Nonterminal],
    best: Option<(Nonterminal, Cost)>,
    rest: &[Nonterminal],
) -> Option<Nonterminal> {
    match (best, rest.split_first()) {
        (Some((nt, c)), Some((nt2 @ (n, _, r), tail))) => {
            let c2 = cost_rule(vnts, std::slice::from_ref(n), r);
            if use_cand(vnts, c, c2, n) {
                sel(vnts, Some(((*nt2).clone(), c2)), tail)
            } else {
                sel(vnts, Some((nt, c)), tail)
            }
        }
        (None, Some((nt2 @ (n, _, r), tail))) => {
            let c2 = cost_rule(vnts, std::slice::from_ref(n), r);
            if use_cand(vnts, Cost::Infinite, c2, n) {
                sel(vnts, Some(((*nt2).clone(), c2)), tail)
            } else {
                sel(vnts, None, tail)
            }
        }
        (Some((nt, _)), None) => Some(nt),
        (None, None) => None,
    }
}

fn use_cand(vnts: &[Nonterminal], cur: Cost, new: Cost, n: &str) -> bool {
    new < cur && under_25(new) && find_nt(n, vnts).is_none()
}

fn under_25(c: Cost) -> bool {
    match c {
        Cost::Finite(x) => x < 25,
        Cost::Infinite => false,
    }
}

fn cost_rule(vnts: &[Nonterminal], visited: &[Identifier], r: &Rule) -> Cost {
    match r {
        Rule::RulePrim(n) => {
            if visited.contains(n) {
                Cost::Infinite
            } else if let Some((_, _, r2)) = find_nt(n, vnts) {
                Cost::Finite(1).add(cost_rule(vnts, &[visited, std::slice::from_ref(n)].concat(), r2))
            } else {
                Cost::Finite(1)
            }
        }
        Rule::RulePos => Cost::Finite(1),
        Rule::RuleChar(_) => Cost::Finite(1),
        Rule::RuleSeq(ms, _) => {
            let mut s = Cost::Finite(1);
            for m in ms {
                s = s.add(match m {
                    Match::MatchAnon(r) => cost_rule(vnts, visited, r),
                    Match::MatchName(r, _) => cost_rule(vnts, visited, r),
                    Match::MatchPat(r, _) => cost_rule(vnts, visited, r),
                    Match::MatchString(r, _) => cost_rule(vnts, visited, r),
                    Match::MatchAnd(r) => cost_rule(vnts, visited, r),
                    Match::MatchNot(r) => cost_rule(vnts, visited, r),
                    Match::MatchPred(_) => Cost::Finite(1),
                });
            }
            s
        }
        Rule::RuleAlt(rs) => {
            let mut a = Cost::Finite(1);
            for r in rs {
                a = a.add(cost_rule(vnts, visited, r));
            }
            a
        }
        Rule::RuleOpt(r) => cost_rule(vnts, visited, r),
        Rule::RuleError(r, _) => cost_rule(vnts, visited, r),
        Rule::RuleString(_) => Cost::Finite(1),
        Rule::RuleStar(r) => cost_rule(vnts, visited, r),
        Rule::RulePlus(r) => cost_rule(vnts, visited, r),
        Rule::RuleExpect(r, _) => cost_rule(vnts, visited, r),
        Rule::RuleSwitchChar(crs, d) => {
            let mut c = Cost::Finite(1);
            for (_, r) in crs {
                c = c.add(cost_rule(vnts, visited, r));
            }
            if let Some(r) = d {
                c = c.add(cost_rule(vnts, visited, r));
            }
            c
        }
        Rule::RuleSwitchString(_, crs, d) => {
            let mut c = Cost::Finite(1);
            for (_, r) in crs {
                c = c.add(cost_rule(vnts, visited, r));
            }
            if let Some(r) = d {
                c = c.add(cost_rule(vnts, visited, r));
            }
            c
        }
    }
}

fn find_nt<'a>(n: &str, nts: &'a [Nonterminal]) -> Option<&'a Nonterminal> {
    nts.iter().find(|(n2, _, _)| n2 == n)
}
