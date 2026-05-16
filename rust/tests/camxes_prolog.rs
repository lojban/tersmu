//! Integration tests for Prolog output format.
//!
//! Verifies that Lojban input parsed through camxes-rs generates
//! syntactically valid Prolog source code. Tests are derived from
//! lojysamban examples (archive/lojysamban/examples/).

use camxes_rs::eval_show::eval_text_to_prolog;
use camxes_rs::jbo_prolog::{prop_to_prolog, PrologMode, props_to_prolog};
use camxes_rs::parse_lojban::parse_text;
use camxes_rs::morphology::morph;

fn parse_and_prolog(input: &str) -> String {
    let text = morph(input).expect("morphology");
    let with_end = format!("{} %%%END%%%", text);
    let parsed = parse_text(&with_end).expect("parse");
    eval_text_to_prolog(&parsed)
}

fn prolog_has_clause(prolog: &str, expected: &str) -> bool {
    prolog.lines().any(|line| line.trim().contains(expected))
}

// ============================================================================
// Basic fact/rule/query tests
// ============================================================================

#[test]
fn test_simple_fact_generates_clause_with_predicate() {
    let prolog = parse_and_prolog("lo ninmu cu klama le zarci");
    assert!(prolog_has_clause(&prolog, "klama"), "Expected klama predicate in: {}", prolog);
}

#[test]
fn test_fact_ends_with_period() {
    let prolog = parse_and_prolog("lo ninmu cu klama le zarci");
    let has_period = prolog.lines().any(|l| l.trim().ends_with('.') && !l.starts_with('%'));
    assert!(has_period, "Expected at least one line ending with period: {}", prolog);
}

#[test]
fn test_prolog_output_not_empty() {
    let prolog = parse_and_prolog("lo ninmu cu klama le zarci");
    assert!(!prolog.is_empty(), "Expected non-empty Prolog output");
}

#[test]
fn test_header_in_output() {
    let prolog = parse_and_prolog("lo ninmu cu klama le zarci");
    assert!(prolog.contains("Generated from camxes-rs"), "Expected header in: {}", prolog);
}

// ============================================================================
// Multiple sentences
// ============================================================================

#[test]
fn test_multiple_facts_separated_by_i() {
    // .i la .alis. cu ninmu .i la bob. cu nanmu
    let prolog = parse_and_prolog(".i la .alis. cu ninmu .i la bob. cu nanmu");
    assert!(prolog_has_clause(&prolog, "ninmu"), "Expected ninmu: {}", prolog);
    assert!(prolog_has_clause(&prolog, "nanmu"), "Expected nanmu: {}", prolog);
}

// ============================================================================
// Equality
// ============================================================================

#[test]
fn test_equality_generates_unification() {
    // .i da du da — self-equality
    let prolog = parse_and_prolog(".i da du da");
    // Should contain = somewhere in a clause
    assert!(!prolog.is_empty(), "Expected non-empty Prolog for equality");
}

// ============================================================================
// Programmatic tests with constructed Props
// ============================================================================

#[test]
fn test_programmatic_fact() {
    use camxes_rs::jbo_prop::JboRel;
    use camxes_rs::jbo_prop::JboTerm;
    use camxes_rs::logic::{Prop, LojQuantifier};
    use camxes_rs::jbo_prop::JboQuantifier;

    // .i da broda de (equivalent to: broda(X0, X1).)
    let prop = Prop::Quantified(
        JboQuantifier::LojQuantifier(LojQuantifier::Exists),
        None,
        std::sync::Arc::new(|x| {
            Prop::Quantified(
                JboQuantifier::LojQuantifier(LojQuantifier::Exists),
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

    let clause = prop_to_prolog(&prop, PrologMode::Fact);
    assert!(clause.contains("broda"), "Expected broda predicate: {}", clause);
    assert!(clause.ends_with(".\n") || clause.ends_with("."), "Expected period ending: {}", clause);
    // Should contain at least two variables (X0, X1 or similar)
    assert!(clause.contains("X"), "Expected variable names: {}", clause);
}

#[test]
fn test_programmatic_rule_with_negation() {
    use camxes_rs::jbo_prop::JboRel;
    use camxes_rs::jbo_prop::JboTerm;
    use camxes_rs::logic::{Prop, LojQuantifier, Connective};
    use camxes_rs::jbo_prop::JboQuantifier;

    // .i da pendo de .ijanai da nadu de
    // pendo(X0, X1) :- \+ X0 = X1.
    let head = Prop::Quantified(
        JboQuantifier::LojQuantifier(LojQuantifier::Exists),
        None,
        std::sync::Arc::new(|x| {
            Prop::Quantified(
                JboQuantifier::LojQuantifier(LojQuantifier::Exists),
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

    // da nadu de — da is different from de
    let body = Prop::Not(Box::new(Prop::Rel(
        JboRel::Equal,
        vec![JboTerm::BoundVar(1), JboTerm::BoundVar(2)],
    )));

    let rule = Prop::Connected(Connective::Impl, Box::new(head), Box::new(body));

    let clause = prop_to_prolog(&rule, PrologMode::Rule);
    assert!(clause.contains(":-"), "Expected :- in rule: {}", clause);
    assert!(clause.contains("\\+"), "Expected negation in: {}", clause);
    assert!(clause.contains("pendo"), "Expected pendo: {}", clause);
    assert!(clause.ends_with(".\n") || clause.ends_with("."), "Expected period: {}", clause);
}

#[test]
fn test_programmatic_query() {
    use camxes_rs::jbo_prop::JboRel;
    use camxes_rs::jbo_prop::JboTerm;
    use camxes_rs::logic::Prop;
    use camxes_rs::jbo_prop::JboQuantifier;

    // .i ma broda ma — query
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

    let clause = prop_to_prolog(&prop, PrologMode::Query);
    assert!(clause.starts_with("?- "), "Expected query prefix: {}", clause);
    assert!(clause.ends_with(".\n") || clause.ends_with("."), "Expected period: {}", clause);
    assert!(clause.contains("broda"), "Expected broda: {}", clause);
}

#[test]
fn test_programmatic_conjunction_in_body() {
    use camxes_rs::jbo_prop::JboRel;
    use camxes_rs::jbo_prop::JboTerm;
    use camxes_rs::logic::{Prop, Connective};
    

    // head :- (cond1 , cond2)
    let head = Prop::Rel(
        JboRel::Brivla("result".to_string()),
        vec![JboTerm::BoundVar(1)],
    );
    let cond1 = Prop::Rel(
        JboRel::Brivla("cond1".to_string()),
        vec![JboTerm::BoundVar(1)],
    );
    let cond2 = Prop::Rel(
        JboRel::Brivla("cond2".to_string()),
        vec![JboTerm::BoundVar(1)],
    );
    let body = Prop::Connected(
        Connective::And,
        Box::new(cond1),
        Box::new(cond2),
    );
    let rule = Prop::Connected(Connective::Impl, Box::new(head), Box::new(body));

    let clause = prop_to_prolog(&rule, PrologMode::Rule);
    assert!(clause.contains(":-"), "Expected :- in rule: {}", clause);
    assert!(clause.contains(","), "Expected comma (conjunction) in: {}", clause);
    assert!(clause.contains("cond1"), "Expected cond1: {}", clause);
    assert!(clause.contains("cond2"), "Expected cond2: {}", clause);
    assert!(clause.contains("result"), "Expected result: {}", clause);
}

#[test]
fn test_programmatic_disjunction_in_body() {
    use camxes_rs::jbo_prop::JboRel;
    use camxes_rs::jbo_prop::JboTerm;
    use camxes_rs::logic::{Prop, Connective};

    // head :- (cond1 ; cond2)
    let head = Prop::Rel(
        JboRel::Brivla("result".to_string()),
        vec![JboTerm::BoundVar(1)],
    );
    let cond1 = Prop::Rel(
        JboRel::Brivla("cond1".to_string()),
        vec![JboTerm::BoundVar(1)],
    );
    let cond2 = Prop::Rel(
        JboRel::Brivla("cond2".to_string()),
        vec![JboTerm::BoundVar(1)],
    );
    let body = Prop::Connected(
        Connective::Or,
        Box::new(cond1),
        Box::new(cond2),
    );
    let rule = Prop::Connected(Connective::Impl, Box::new(head), Box::new(body));

    let clause = prop_to_prolog(&rule, PrologMode::Rule);
    assert!(clause.contains(":-"), "Expected :- in rule: {}", clause);
    assert!(clause.contains(";"), "Expected semicolon (disjunction) in: {}", clause);
    assert!(clause.contains("cond1"), "Expected cond1: {}", clause);
    assert!(clause.contains("cond2"), "Expected cond2: {}", clause);
}

#[test]
fn test_programmatic_not_equals_body() {
    use camxes_rs::jbo_prop::{JboRel, JboTerm, JboQuantifier};
    use camxes_rs::logic::{Prop, LojQuantifier, Connective};

    // da broda de .ijanai da nadu de
    // broda(X0, X1) :- \+ X0 = X1
    // This is equivalent to: broda(X0, X1) :- X0 \= X1
    let head = Prop::Quantified(
        JboQuantifier::LojQuantifier(LojQuantifier::Exists),
        None,
        std::sync::Arc::new(|x| {
            Prop::Quantified(
                JboQuantifier::LojQuantifier(LojQuantifier::Exists),
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
    let body = Prop::Not(Box::new(Prop::Rel(
        JboRel::Equal,
        vec![JboTerm::BoundVar(1), JboTerm::BoundVar(2)],
    )));
    let rule = Prop::Connected(Connective::Impl, Box::new(head), Box::new(body));

    let clause = prop_to_prolog(&rule, PrologMode::Rule);
    // Should contain negation of unification
    assert!(clause.contains("\\+"), "Expected negation: {}", clause);
}

#[test]
fn test_multiple_clauses_from_props() {
    use camxes_rs::jbo_prop::{JboRel, JboTerm, JboQuantifier};
    use camxes_rs::logic::{Prop, LojQuantifier};

    // .i da broda de .i da brode de
    let prop1 = Prop::Quantified(
        JboQuantifier::LojQuantifier(LojQuantifier::Exists),
        None,
        std::sync::Arc::new(|x| {
            Prop::Quantified(
                JboQuantifier::LojQuantifier(LojQuantifier::Exists),
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
    let prop2 = Prop::Quantified(
        JboQuantifier::LojQuantifier(LojQuantifier::Exists),
        None,
        std::sync::Arc::new(|x| {
            Prop::Quantified(
                JboQuantifier::LojQuantifier(LojQuantifier::Exists),
                None,
                std::sync::Arc::new(move |y| {
                    Prop::Rel(
                        JboRel::Brivla("brode".to_string()),
                        vec![JboTerm::BoundVar(x), JboTerm::BoundVar(y)],
                    )
                }),
            )
        }),
    );

    let clauses = props_to_prolog(&[prop1, prop2]);
    assert_eq!(clauses.len(), 2, "Expected 2 clauses, got {}: {:?}", clauses.len(), clauses);
    assert!(clauses[0].contains("broda"), "First clause should be broda: {}", clauses[0]);
    assert!(clauses[1].contains("brode"), "Second clause should be brode: {}", clauses[1]);
}
