//! Prolog output format for camxes-rs semantic propositions.
//!
//! Converts [JboProp](crate::jbo_prop::JboProp) to SWI-Prolog compatible source code,
//! following the model established by lojysamban (Haskell, 2012) and targetting
//! feature parity with Logical English.

use std::collections::BTreeMap;

use crate::jbo_prop::{JboMex, JboModalOp, JboOperator, JboProp, JboQuantifier, JboRel, JboTerm, Texticule};
use crate::logic::Prop;

/// Tracks bound variable → Prolog variable name mapping during conversion.
#[derive(Default, Clone)]
struct VarCtx {
    /// Maps BoundVar index to Prolog variable name (e.g. 1 → "X1")
    mapping: BTreeMap<i32, String>,
    next_var: usize,
}

impl VarCtx {
    fn new() -> Self {
        Self::default()
    }

    fn var_for(&mut self, n: i32) -> String {
        if let Some(name) = self.mapping.get(&n) {
            return name.clone();
        }
        // Assign next variable name
        let name = format!("X{}", self.next_var);
        self.mapping.insert(n, name.clone());
        self.next_var += 1;
        name
    }

    /// Ensure a BoundVar has a name (used for scanning before output)
    fn ensure(&mut self, n: i32) {
        self.var_for(n);
    }
}

/// Output mode for proposition conversion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrologMode {
    /// Top-level fact: `pred(terms...).`
    Fact,
    /// Rule: `head :- body.`
    Rule,
    /// Query: `?- body.`
    Query,
    /// Inside rule body: no trailing period
    Body,
}

// ============================================================================
// Public API
// ============================================================================

/// Convert a [JboProp] to a Prolog clause string.
pub fn prop_to_prolog(prop: &JboProp, mode: PrologMode) -> String {
    let mut ctx = VarCtx::new();
    scan_prop(prop, &mut ctx);
    let body = convert_prop(prop, &mut ctx);
    if body.is_empty() {
        return String::new();
    }
    match mode {
        PrologMode::Fact | PrologMode::Rule => {
            if !body.ends_with('.') {
                format!("{}.", body)
            } else {
                body
            }
        }
        PrologMode::Query => format!("?- {}.", body),
        PrologMode::Body => body,
    }
}

/// Convert multiple propositions (from separate `.i` sentences) to Prolog.
/// Returns a vector of clause strings.
pub fn props_to_prolog(props: &[JboProp]) -> Vec<String> {
    let mut clauses = Vec::new();
    for prop in props {
        let clause = prop_to_prolog_clause(prop);
        if !clause.is_empty() {
            clauses.push(clause);
        }
    }
    clauses
}

/// Convert a top-level [JboProp] into a Prolog clause, detecting mode automatically.
fn prop_to_prolog_clause(prop: &JboProp) -> String {
    let mut ctx = VarCtx::new();
    scan_prop(prop, &mut ctx);
    let body = convert_prop(prop, &mut ctx);
    if body.is_empty() {
        return String::new();
    }
    match detect_mode(prop) {
        PrologMode::Query => format!("?- {}.", body),
        _ => {
            if !body.ends_with('.') {
                format!("{}.", body)
            } else {
                body
            }
        }
    }
}

/// Detect whether a top-level proposition is a query, rule, or fact.
fn detect_mode(prop: &JboProp) -> PrologMode {
    match prop {
        Prop::Quantified(JboQuantifier::QuestionQuantifier, _, _) => PrologMode::Query,
        Prop::Quantified(_, _, body) => detect_mode(&body(-1)),
        Prop::Connected(crate::logic::Connective::Impl, _, _) => PrologMode::Rule,
        _ if has_question_quantifier(prop) => PrologMode::Query,
        _ => PrologMode::Fact,
    }
}

/// Check if any sub-proposition contains a question quantifier.
fn has_question_quantifier(prop: &JboProp) -> bool {
    match prop {
        Prop::Quantified(JboQuantifier::QuestionQuantifier, _, _) => true,
        Prop::Quantified(JboQuantifier::RelQuantifier(q), _, _)
            if matches!(q.as_ref(), JboQuantifier::QuestionQuantifier) =>
        {
            true
        }
        Prop::Quantified(_, _, body) => has_question_quantifier(&body(-1)),
        Prop::Not(p) | Prop::Modal(_, p) => has_question_quantifier(p),
        Prop::Connected(_, p1, p2) | Prop::NonLogConnected(_, p1, p2) => {
            has_question_quantifier(p1) || has_question_quantifier(p2)
        }
        Prop::Rel(JboRel::UnboundBribasti(_), _) => true,
        _ => false,
    }
}

/// Convert a rule body (handles conjunctions).
fn convert_body(prop: &JboProp, ctx: &mut VarCtx) -> String {
    flatten_and_to_clauses(prop, ctx)
}

/// Flatten nested AND connectives into a comma-separated list of Prolog goals.
fn flatten_and_to_clauses(prop: &JboProp, ctx: &mut VarCtx) -> String {
    match prop {
        Prop::Connected(crate::logic::Connective::And, p1, p2) => {
            let left = flatten_and_to_clauses(p1, ctx);
            let right = flatten_and_to_clauses(p2, ctx);
            if left.is_empty() {
                right
            } else if right.is_empty() {
                left
            } else {
                format!("{}, {}", left, right)
            }
        }
        _ => convert_prop(prop, ctx),
    }
}

/// Flatten nested OR connectives into a semicolon-separated list.
fn flatten_or_to_clauses(prop: &JboProp, ctx: &mut VarCtx) -> String {
    match prop {
        Prop::Connected(crate::logic::Connective::Or, p1, p2) => {
            let left = flatten_or_to_clauses(p1, ctx);
            let right = flatten_or_to_clauses(p2, ctx);
            if left.is_empty() {
                right
            } else if right.is_empty() {
                left
            } else {
                format!("({} ; {})", left, right)
            }
        }
        _ => convert_prop(prop, ctx),
    }
}

// ============================================================================
// Scanning: collect all BoundVars before conversion for consistent naming
// ============================================================================

fn scan_prop(prop: &JboProp, ctx: &mut VarCtx) {
    match prop {
        Prop::Rel(rel, terms) => {
            scan_rel(rel, ctx);
            for term in terms {
                scan_term(term, ctx);
            }
        }
        Prop::Not(p) | Prop::Modal(_, p) => scan_prop(p, ctx),
        Prop::Connected(_, p1, p2) | Prop::NonLogConnected(_, p1, p2) => {
            scan_prop(p1, ctx);
            scan_prop(p2, ctx);
        }
        Prop::Quantified(_, restriction, body) => {
            if let Some(r) = restriction {
                scan_prop(&r(-1), ctx);
            }
            scan_prop(&body(-1), ctx);
        }
        Prop::Eet => {}
    }
}

fn scan_rel(rel: &JboRel, ctx: &mut VarCtx) {
    match rel {
        JboRel::Among(t) | JboRel::Moi(t, _) => scan_term(t, ctx),
        JboRel::Tanru(r1, r2) | JboRel::TanruConnective(_, r1, r2) => {
            scan_rel(r1, ctx);
            scan_rel(r2, ctx);
        }
        JboRel::AppliedRel(r, terms) => {
            scan_rel(r, ctx);
            for term in terms {
                scan_term(term, ctx);
            }
        }
        JboRel::PermutedRel(_, r) | JboRel::ScalarNegatedRel(_, r) | JboRel::ModalRel(_, r) => {
            scan_rel(r, ctx);
        }
        JboRel::OperatorRel(op) => scan_operator(op, ctx),
        JboRel::AbsPred(_, npred) => {
            let args: Vec<JboTerm> = (1..=npred.arity as i32).map(|n| JboTerm::BoundVar(-n)).collect();
            scan_prop(&(npred.pred)(&args), ctx);
        }
        JboRel::AbsProp(_, p) => scan_prop(p, ctx),
        _ => {}
    }
}

fn scan_term(term: &JboTerm, ctx: &mut VarCtx) {
    match term {
        JboTerm::BoundVar(n) => {
            ctx.ensure(*n);
        }
        JboTerm::Constant(_, args) => {
            for arg in args {
                scan_term(arg, ctx);
            }
        }
        JboTerm::QualifiedTerm(_, t) | JboTerm::TermWithSides(t, _) => scan_term(t, ctx),
        JboTerm::JoikedTerms(_, t1, t2) => {
            scan_term(t1, ctx);
            scan_term(t2, ctx);
        }
        JboTerm::Value(m) => scan_mex(m, ctx),
        _ => {}
    }
}

fn scan_mex(mex: &JboMex, ctx: &mut VarCtx) {
    match mex {
        JboMex::Operation(op, args) => {
            scan_operator(op, ctx);
            for arg in args {
                scan_mex(arg, ctx);
            }
        }
        JboMex::ConnectedMex(_, _, m1, m2) => {
            scan_mex(m1, ctx);
            scan_mex(m2, ctx);
        }
        JboMex::QualifiedMex(_, m) => scan_mex(m, ctx),
        JboMex::MexSumti(t) => scan_term(t, ctx),
        JboMex::MexArray(ms) => {
            for m in ms {
                scan_mex(m, ctx);
            }
        }
        _ => {}
    }
}

fn scan_operator(op: &JboOperator, ctx: &mut VarCtx) {
    match op {
        JboOperator::ConnectedOperator(_, _, o1, o2) => {
            scan_operator(o1, ctx);
            scan_operator(o2, ctx);
        }
        JboOperator::OpPermuted(_, o) | JboOperator::OpScalarNegated(_, o) => scan_operator(o, ctx),
        JboOperator::OpMex(m) => scan_mex(m, ctx),
        _ => {}
    }
}

// ============================================================================
// Conversion: JboProp → Prolog string
// ============================================================================

fn convert_prop(prop: &JboProp, ctx: &mut VarCtx) -> String {
    match prop {
        Prop::Rel(rel, terms) => convert_rel_prop(rel, terms, ctx),
        Prop::Not(p) => format!("\\+ {}", convert_prop(p, ctx)),
        Prop::Connected(crate::logic::Connective::And, p1, p2) => {
            let left = convert_prop(p1, ctx);
            let right = convert_prop(p2, ctx);
            if left.is_empty() { right }
            else if right.is_empty() { left }
            else { format!("({} , {})", left, right) }
        }
        Prop::Connected(crate::logic::Connective::Or, p1, p2) => {
            let left = convert_prop(p1, ctx);
            let right = convert_prop(p2, ctx);
            if left.is_empty() { right }
            else if right.is_empty() { left }
            else { format!("({} ; {})", left, right) }
        }
        Prop::Connected(crate::logic::Connective::Impl, head, body) => {
            let head_str = convert_prop(head, ctx);
            let body_str = convert_prop(body, ctx);
            if head_str.is_empty() || body_str.is_empty() {
                String::new()
            } else {
                format!("{} :- {}", head_str, body_str)
            }
        }
        Prop::Connected(crate::logic::Connective::Equiv, p1, p2) => {
            let left = convert_prop(p1, ctx);
            let right = convert_prop(p2, ctx);
            format!("({} , {})", left, right)
        }
        Prop::NonLogConnected(joik, p1, p2) => {
            let left = convert_prop(p1, ctx);
            let right = convert_prop(p2, ctx);
            format!("{}_joik_({} , {})", joik, left, right)
        }
        Prop::Modal(JboModalOp::NonVeridical, p) => {
            // Non-veridical: not not (approximation)
            format!("\\+ \\+ {}", convert_prop(p, ctx))
        }
        Prop::Modal(JboModalOp::QTruthModal, p) => {
            // Truth question — strip the modal for Prolog
            convert_prop(p, ctx)
        }
        Prop::Modal(JboModalOp::WithEventAs(t), p) => {
            // Event identity
            let term = convert_term(t, ctx);
            let inner = convert_prop(p, ctx);
            format!("{} = event_of({})", term, inner)
        }
        Prop::Modal(JboModalOp::Tagged(_tag, _mt), p) => {
            // Tagged modal — approximate by unwrapping
            convert_prop(p, ctx)
        }
        Prop::Quantified(JboQuantifier::QuestionQuantifier, restriction, body) => {
            let n = 1i32;
            let _var_name = ctx.var_for(n);
            let mut prefix = String::new();
            if let Some(r) = restriction {
                let restriction_str = convert_prop(&r(n), ctx);
                prefix.push_str(&format!("{}, ", restriction_str));
            }
            let body_str = convert_prop(&body(n), ctx);
            if prefix.is_empty() {
                body_str
            } else {
                format!("{}{}", prefix, body_str)
            }
        }
        Prop::Quantified(JboQuantifier::RelQuantifier(_inner_q), _restriction, body) => {
            convert_prop(&body(-1), ctx)
        }
        Prop::Quantified(_q, restriction, body) => {
            let n = 1i32;
            let _var_name = ctx.var_for(n);
            let mut goals = Vec::new();
            if let Some(r) = restriction {
                let restriction_str = convert_prop(&r(n), ctx);
                if !restriction_str.is_empty() {
                    goals.push(restriction_str);
                }
            }
            let body_str = convert_prop(&body(n), ctx);
            if !body_str.is_empty() {
                goals.push(body_str);
            }
            goals.join(", ")
        }
        Prop::Eet => "true".to_string(),
    }
}

fn convert_rel_prop(rel: &JboRel, terms: &[JboTerm], ctx: &mut VarCtx) -> String {
    match rel {
        JboRel::Equal => {
            if terms.len() >= 2 {
                format!(
                    "{} = {}",
                    convert_term(&terms[0], ctx),
                    convert_term(&terms[1], ctx)
                )
            } else {
                String::new()
            }
        }
        JboRel::Among(set) => {
            if let Some(t) = terms.first() {
                format!("member({}, {})", convert_term(t, ctx), convert_term(set, ctx))
            } else {
                String::new()
            }
        }
        JboRel::Tanru(seltau, tertau) => {
            // Tanru: introduce a shared variable via conjunction
            let shared_var = format!("Tanru{}", ctx.next_var);
            ctx.next_var += 1;
            let seltau_str = convert_rel_as_goal(seltau, &shared_var, ctx);
            let tertau_str = convert_rel_with_args(tertau, terms, &shared_var, ctx);
            if seltau_str.is_empty() {
                tertau_str
            } else if tertau_str.is_empty() {
                seltau_str
            } else {
                format!("{}, {}", seltau_str, tertau_str)
            }
        }
        JboRel::AppliedRel(r, applied_terms) => {
            // Relation with already-filled places
            let mut all_terms = applied_terms.clone();
            all_terms.extend_from_slice(terms);
            convert_rel_prop(r, &all_terms, ctx)
        }
        JboRel::TanruConnective(_con, r1, r2) => {
            // Connected tanru: use shared variable
            let shared_var = format!("Tcon{}", ctx.next_var);
            ctx.next_var += 1;
            let seltau_str = convert_rel_as_goal(r1, &shared_var, ctx);
            let tertau_str = convert_rel_with_args(r2, terms, &shared_var, ctx);
            format!("{}, {}", seltau_str, tertau_str)
        }
        _ => {
            // Default: relation with terms as arguments
            let pred = convert_rel_name(rel);
            let args_str: Vec<String> = terms.iter()
                .filter(|t| !matches!(t, JboTerm::Unfilled))
                .map(|t| convert_term(t, ctx))
                .collect();
            if args_str.is_empty() {
                pred
            } else {
                format!("{}({})", pred, args_str.join(", "))
            }
        }
    }
}

/// Convert a relation name for Prolog predicate.
fn convert_rel_name(rel: &JboRel) -> String {
    match rel {
        JboRel::Brivla(s) => escape_prolog_atom(s),
        JboRel::Equal => "=".to_string(),
        JboRel::PermutedRel(n, r) => {
            let inner = convert_rel_name(r);
            format!("{}_se{}", inner, n)
        }
        JboRel::ScalarNegatedRel(nahe, r) => {
            let inner = convert_rel_name(r);
            format!("{}_{}", inner, nahe)
        }
        _ => escape_prolog_atom(&format!("{:?}", rel)),
    }
}

/// Convert a relation as a goal with a shared argument (for tanru composition).
fn convert_rel_as_goal(rel: &JboRel, shared_var: &str, ctx: &mut VarCtx) -> String {
    match rel {
        JboRel::Brivla(s) => {
            format!("{}({})", escape_prolog_atom(s), shared_var)
        }
        JboRel::Among(set) => {
            format!("member({}, {})", shared_var, convert_term(set, ctx))
        }
        JboRel::AppliedRel(r, terms) => {
            let mut all_terms = terms.clone();
            all_terms.push(JboTerm::NonAnaph(shared_var.to_string()));
            convert_rel_prop(r, &all_terms, ctx)
        }
        _ => {
            let pred = convert_rel_name(rel);
            format!("{}({})", pred, shared_var)
        }
    }
}

/// Convert a relation with explicit terms + a shared first argument.
fn convert_rel_with_args(rel: &JboRel, terms: &[JboTerm], shared_var: &str, ctx: &mut VarCtx) -> String {
    match rel {
        JboRel::Brivla(s) => {
            let mut args = vec![shared_var.to_string()];
            args.extend(terms.iter()
                .filter(|t| !matches!(t, JboTerm::Unfilled))
                .map(|t| convert_term(t, ctx)));
            format!("{}({})", escape_prolog_atom(s), args.join(", "))
        }
        JboRel::AppliedRel(r, applied_terms) => {
            let mut all_terms = applied_terms.clone();
            all_terms.extend_from_slice(terms);
            convert_rel_with_args(r, &all_terms, shared_var, ctx)
        }
        _ => {
            let pred = convert_rel_name(rel);
            let mut args = vec![shared_var.to_string()];
            args.extend(terms.iter()
                .filter(|t| !matches!(t, JboTerm::Unfilled))
                .map(|t| convert_term(t, ctx)));
            format!("{}({})", pred, args.join(", "))
        }
    }
}

// ============================================================================
// Conversion: JboTerm → Prolog term string
// ============================================================================

fn convert_term(term: &JboTerm, ctx: &mut VarCtx) -> String {
    match term {
        JboTerm::BoundVar(n) => ctx.var_for(*n),
        JboTerm::Var(_) => "_".to_string(),
        JboTerm::Named(s) => escape_prolog_atom(s),
        JboTerm::NonAnaph(s) => escape_prolog_atom(s),
        JboTerm::Unfilled => "_".to_string(),
        JboTerm::Constant(n, args) => {
            if args.is_empty() {
                format!("const_{}", n)
            } else {
                let args_str: Vec<String> = args.iter().map(|t| convert_term(t, ctx)).collect();
                format!("const_{}({})", n, args_str.join(", "))
            }
        }
        JboTerm::Value(mex) => convert_mex_to_term(mex, ctx),
        JboTerm::QualifiedTerm(_, t) | JboTerm::TermWithSides(t, _) => convert_term(t, ctx),
        JboTerm::JoikedTerms(joik, t1, t2) => {
            if joik == "ce" || joik == "ce'o" {
                // List construction
                if matches!(t2.as_ref(), JboTerm::JoikedTerms(_, _, _)) {
                    format!("[{}|{}]", convert_term(t1, ctx), convert_list_tail(t2, ctx))
                } else {
                    format!("[{}|{}]", convert_term(t1, ctx), convert_term(t2, ctx))
                }
            } else {
                // Collective: represent as compound
                format!("joik_{}({}, {})", joik, convert_term(t1, ctx), convert_term(t2, ctx))
            }
        }
        JboTerm::UnboundSumbasti(_) => "unbound".to_string(),
        JboTerm::PredNamed(pred) => {
            let prop = pred(&JboTerm::BoundVar(-1));
            convert_prop(&prop, ctx)
        }
        JboTerm::JboQuote(_) | JboTerm::JboErrorQuote(_) | JboTerm::JboNonJboQuote(_) => {
            "_".to_string()
        }
        JboTerm::Valsi(s) => escape_prolog_atom(s),
        JboTerm::TheMex(_) => "_".to_string(),
    }
}

/// Convert a nested joiked term into a list tail.
fn convert_list_tail(term: &JboTerm, ctx: &mut VarCtx) -> String {
    match term {
        JboTerm::JoikedTerms(joik, t1, t2) if joik == "ce" || joik == "ce'o" => {
            if matches!(t2.as_ref(), JboTerm::JoikedTerms(_, _, _)) {
                format!("[{}|{}]", convert_term(t1, ctx), convert_list_tail(t2, ctx))
            } else {
                format!("[{}|{}]", convert_term(t1, ctx), convert_term(t2, ctx))
            }
        }
        _ => convert_term(term, ctx),
    }
}

fn convert_mex_to_term(mex: &JboMex, ctx: &mut VarCtx) -> String {
    match mex {
        JboMex::MexInt(n) => n.to_string(),
        JboMex::MexNumeralString(ns) => {
            let s: String = ns.iter().map(|n| match n {
                crate::jbo_syntax::Numeral::PA(s) => s.as_str(),
                crate::jbo_syntax::Numeral::NumeralLerfu(_) => "?",
            }).collect();
            s
        }
        JboMex::MexLerfuString(_) => "_".to_string(),
        JboMex::Operation(op, args) => {
            let args_str: Vec<String> = args.iter().map(|m| convert_mex_to_term(m, ctx)).collect();
            format!("{}({})", convert_operator_name(op), args_str.join(", "))
        }
        JboMex::MexSumti(t) => convert_term(t, ctx),
        JboMex::MexArray(ms) => {
            let items: Vec<String> = ms.iter().map(|m| convert_mex_to_term(m, ctx)).collect();
            format!("[{}]", items.join(", "))
        }
        JboMex::ConnectedMex(_, con, m1, m2) => {
            let conn = match con {
                crate::jbo_syntax::Connective::JboConnLog(_, lcon) => format!("{}", lcon.c),
                crate::jbo_syntax::Connective::JboConnJoik(_, j) => j.clone(),
            };
            format!("{}_{}_{}", conn, convert_mex_to_term(m1, ctx), convert_mex_to_term(m2, ctx))
        }
        JboMex::QualifiedMex(_, m) => convert_mex_to_term(m, ctx),
        JboMex::MexSelbri(vpred) => {
            let prop = vpred(&[JboTerm::BoundVar(-1)]);
            convert_prop(&prop, ctx)
        }
    }
}

fn convert_operator_name(op: &JboOperator) -> String {
    match op {
        JboOperator::OpVUhU(s) => escape_prolog_atom(s),
        _ => escape_prolog_atom(&format!("{:?}", op)),
    }
}

// ============================================================================
// Prolog text helpers
// ============================================================================

/// Escape a string for use as a Prolog atom (single-quoted).
fn escape_prolog_atom(s: &str) -> String {
    if s.is_empty() {
        return "''".to_string();
    }
    // Check if the string can be an unquoted atom
    if can_be_unquoted_atom(s) {
        return s.to_string();
    }
    let escaped = s.replace('\'', "''");
    format!("'{}'", escaped)
}

/// Check if a string can be a bare Prolog atom (lowercase letter start, alphanumeric + underscore).
fn can_be_unquoted_atom(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_lowercase() => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Generate a Prolog header comment.
pub fn prolog_header(source: &str) -> String {
    format!("% Generated from camxes-rs\n% Source: {}\n\n", source)
}


// ============================================================================
// Text-level conversion (for use with eval_show / CLI pipeline)
// ============================================================================

/// Convert a sequence of semantic results to Prolog source.
/// Uses `side_props` + `prop` from each SemanticResult.
pub fn semantic_results_to_prolog(
    results: &[crate::jbo_parse::SemanticResult],
) -> String {
    let mut clauses = Vec::new();
    clauses.push(prolog_header("camxes-rs CLI"));

    for result in results {
        // Process side propositions first
        for side_prop in &result.side_props {
            if let Some(clause) = convert_side_prop_to_prolog(side_prop) {
                clauses.push(clause);
            }
        }
        // Process main proposition
        let clause = prop_to_prolog_clause(&result.prop);
        if !clause.is_empty() {
            clauses.push(clause);
        }
    }

    clauses.join("\n")
}

/// Convert a side (discursive) proposition to Prolog comment or clause.
fn convert_side_prop_to_prolog(prop: &JboProp) -> Option<String> {
    let clause = prop_to_prolog_clause(prop);
    if clause.is_empty() {
        None
    } else {
        Some(format!("% side: {}", clause))
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_prolog_atom() {
        assert_eq!(escape_prolog_atom("broda"), "broda");
        assert_eq!(escape_prolog_atom("klama"), "klama");
        assert_eq!(escape_prolog_atom("Broda"), "'Broda'");
        assert_eq!(escape_prolog_atom("a'b"), "'a''b'");
        assert_eq!(escape_prolog_atom(""), "''");
        assert_eq!(escape_prolog_atom("mi"), "mi");
        assert_eq!(escape_prolog_atom("patfu"), "patfu");
        assert_eq!(escape_prolog_atom("cinfo"), "cinfo");
        assert_eq!(escape_prolog_atom("tirxu"), "tirxu");
        assert_eq!(escape_prolog_atom(".irci."), "'.irci.'");
    }

    #[test]
    fn test_simple_fact() {
        // .i mi klama le zarci
        let prop = Prop::Rel(
            JboRel::Brivla("klama".to_string()),
            vec![
                JboTerm::NonAnaph("mi".to_string()),
                JboTerm::BoundVar(1),
            ],
        );
        let result = prop_to_prolog(&prop, PrologMode::Fact);
        assert!(result.starts_with("klama(mi, X0)"), "Got: {}", result);
        assert!(result.ends_with("."), "Got: {}", result);
    }

    #[test]
    fn test_basic_rule() {
        // .i da pendo de .ijanai da nadu de
        // Actually tests: pendo(X0, X1) :- \+ X0 = X1
        let head = Prop::Quantified(
            JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists),
            None,
            std::sync::Arc::new(|x| {
                Prop::Quantified(
                    JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists),
                    None,
                    std::sync::Arc::new(move |y| {
                        Prop::Rel(
                            JboRel::Brivla("pendo".to_string()),
                            vec![JboTerm::BoundVar(x), JboTerm::BoundVar(y)],
                        )
                    }),
                )
            }),
        );
        let body = Prop::Not(Box::new(Prop::Rel(
            JboRel::Equal,
            vec![JboTerm::BoundVar(1), JboTerm::BoundVar(2)],
        )));
        let rule = Prop::Connected(
            crate::logic::Connective::Impl,
            Box::new(head),
            Box::new(body),
        );
        // Test that it generates valid Prolog
        let result = prop_to_prolog(&rule, PrologMode::Rule);
        assert!(!result.is_empty(), "Expected non-empty Prolog rule");
        // Should contain :- and \+
        assert!(result.contains(":-"), "Expected :- in rule, got: {}", result);
        assert!(result.contains("\\+"), "Expected \\+ in rule, got: {}", result);
    }

    #[test]
    fn test_fact_with_two_vars() {
        // .i da broda de — two existentially bound vars
        let prop = Prop::Quantified(
            JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists),
            None,
            std::sync::Arc::new(|x| {
                Prop::Quantified(
                    JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists),
                    None,
                    std::sync::Arc::new(move |y| {
                        Prop::Rel(
                            JboRel::Brivla("broda".to_string()),
                            vec![JboTerm::BoundVar(x), JboTerm::BoundVar(y)],
                        )
                    }),
                )
            }),
        );
        let result = prop_to_prolog(&prop, PrologMode::Fact);
        println!("Fact result: {}", result);
        // Should be something like "broda(X0, X1)."
        assert!(result.contains("broda"), "Expected broda in result: {}", result);
        assert!(result.ends_with("."), "Expected period at end: {}", result);
    }

    #[test]
    fn test_query_detection() {
        // .i ma broda ma
        let prop = Prop::Quantified(
            JboQuantifier::QuestionQuantifier,
            None,
            std::sync::Arc::new(|x| {
                Prop::Quantified(
                    JboQuantifier::QuestionQuantifier,
                    None,
                    std::sync::Arc::new(move |y| {
                        Prop::Rel(
                            JboRel::Brivla("broda".to_string()),
                            vec![JboTerm::BoundVar(x), JboTerm::BoundVar(y)],
                        )
                    }),
                )
            }),
        );
        assert_eq!(detect_mode(&prop), PrologMode::Query);
    }

    #[test]
    fn test_equality() {
        // da du da
        let prop = Prop::Rel(
            JboRel::Equal,
            vec![JboTerm::BoundVar(1), JboTerm::BoundVar(1)],
        );
        let result = prop_to_prolog(&prop, PrologMode::Fact);
        assert!(result.contains("X0 = X0"), "Expected X0 = X0, got: {}", result);
    }

    #[test]
    fn test_prop_to_prolog_clause_detects_rule() {
        // da broda de .ijanai da du de
        let head = Prop::Quantified(
            JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists),
            None,
            std::sync::Arc::new(|x| {
                Prop::Quantified(
                    JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists),
                    None,
                    std::sync::Arc::new(move |y| {
                        Prop::Rel(
                            JboRel::Brivla("broda".to_string()),
                            vec![JboTerm::BoundVar(x), JboTerm::BoundVar(y)],
                        )
                    }),
                )
            }),
        );
        let body = Prop::Rel(
            JboRel::Equal,
            vec![JboTerm::BoundVar(1), JboTerm::BoundVar(2)],
        );
        let rule = Prop::Connected(
            crate::logic::Connective::Impl,
            Box::new(head),
            Box::new(body),
        );
        let clause = prop_to_prolog_clause(&rule);
        assert!(clause.contains(":-"), "Expected :- in clause: {}", clause);
        assert!(clause.ends_with("."), "Expected period at end: {}", clause);
    }
}
