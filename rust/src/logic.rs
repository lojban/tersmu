//! Proposition type from [Logic.hs](../Logic.hs). Quantified formulas use higher-order abstract
//! syntax in Haskell; `Quantified` is added when [JboParse.hs](../JboParse.hs) is ported.

use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Connective {
    And,
    Or,
    Impl,
    Equiv,
}

impl std::fmt::Display for Connective {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Connective::And => "∧",
            Connective::Or => "∨",
            Connective::Impl => "→",
            Connective::Equiv => "↔",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LojQuantifier {
    Exists,
    Forall,
    Exactly(i32),
}

impl std::fmt::Display for LojQuantifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LojQuantifier::Exists => write!(f, "∃"),
            LojQuantifier::Forall => write!(f, "∀"),
            LojQuantifier::Exactly(n) => write!(f, "EQ({n})"),
        }
    }
}

pub enum Prop<R, T, C, O, Q> {
    Not(Box<Prop<R, T, C, O, Q>>),
    Connected(
        Connective,
        Box<Prop<R, T, C, O, Q>>,
        Box<Prop<R, T, C, O, Q>>,
    ),
    NonLogConnected(C, Box<Prop<R, T, C, O, Q>>, Box<Prop<R, T, C, O, Q>>),
    Modal(O, Box<Prop<R, T, C, O, Q>>),
    /// Quantified proposition with quantifier, optional predicate, and body function
    Quantified(Q, Option<PropFn<R, T, C, O, Q>>, PropFn<R, T, C, O, Q>),
    Rel(R, Vec<T>),
    Eet,
}

type PropFn<R, T, C, O, Q> = Rc<dyn Fn(i32) -> Prop<R, T, C, O, Q>>;

impl<R: Clone, T: Clone, C: Clone, O: Clone, Q: Clone> Clone for Prop<R, T, C, O, Q> {
    fn clone(&self) -> Self {
        match self {
            Prop::Not(p) => Prop::Not(p.clone()),
            Prop::Connected(c, p1, p2) => Prop::Connected(*c, p1.clone(), p2.clone()),
            Prop::NonLogConnected(c, p1, p2) => Prop::NonLogConnected(c.clone(), p1.clone(), p2.clone()),
            Prop::Modal(o, p) => Prop::Modal(o.clone(), p.clone()),
            Prop::Quantified(q, r, f) => Prop::Quantified(q.clone(), r.clone(), f.clone()),
            Prop::Rel(r, ts) => Prop::Rel(r.clone(), ts.clone()),
            Prop::Eet => Prop::Eet,
        }
    }
}

impl<R: std::fmt::Debug, T: std::fmt::Debug, C: std::fmt::Debug, O: std::fmt::Debug, Q: std::fmt::Debug> std::fmt::Debug for Prop<R, T, C, O, Q> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prop::Not(p) => f.debug_tuple("Not").field(p).finish(),
            Prop::Connected(c, p1, p2) => f.debug_tuple("Connected").field(c).field(p1).field(p2).finish(),
            Prop::NonLogConnected(c, p1, p2) => f.debug_tuple("NonLogConnected").field(c).field(p1).field(p2).finish(),
            Prop::Modal(o, p) => f.debug_tuple("Modal").field(o).field(p).finish(),
            Prop::Quantified(q, _, _) => f.debug_tuple("Quantified").field(q).field(&"<fn>").finish(),
            Prop::Rel(r, ts) => f.debug_tuple("Rel").field(r).field(ts).finish(),
            Prop::Eet => write!(f, "Eet"),
        }
    }
}

// ============================================================================
// Functions from Logic.hs
// ============================================================================

// Ported from: Logic.hs :: bigAnd
/// Combine multiple propositions with conjunction
pub fn big_and<R: Clone, T: Clone, C: Clone, O: Clone, Q: Clone>(
    props: Vec<Prop<R, T, C, O, Q>>
) -> Prop<R, T, C, O, Q> {
    // Filter out Not(Eet) propositions
    let filtered: Vec<_> = props.into_iter()
        .filter(|p| !matches!(p, Prop::Not(inner) if matches!(**inner, Prop::Eet)))
        .collect();

    big_and_inner(filtered)
}

// Ported from: Logic.hs :: bigAnd (recursive helper)
fn big_and_inner<R: Clone, T: Clone, C: Clone, O: Clone, Q: Clone>(
    props: Vec<Prop<R, T, C, O, Q>>
) -> Prop<R, T, C, O, Q> {
    match props.len() {
        0 => Prop::Not(Box::new(Prop::Eet)),
        1 => props.into_iter().next().unwrap(),
        _ => {
            let mut iter = props.into_iter();
            let first = iter.next().unwrap();
            let rest: Vec<_> = iter.collect();
            Prop::Connected(
                Connective::And,
                Box::new(first),
                Box::new(big_and_inner(rest))
            )
        }
    }
}

// Ported from: Logic.hs :: terpProp
/// Lift an interpretation of atomic formulae, operators, and quantifiers to an interpretation of arbitrary formula
pub fn terp_prop<R1, T1, C, O1, Q1, R2, T2, O2, Q2, A, OP, QP>(
    terp_atomic: A,
    terp_op: OP,
    terp_q: QP,
    prop: &Prop<R1, T1, C, O1, Q1>,
) -> Prop<R2, T2, C, O2, Q2>
where
    R1: 'static,
    T1: 'static,
    C: Clone + 'static,
    O1: 'static,
    Q1: 'static,
    R2: Clone + 'static,
    T2: Clone + 'static,
    O2: Clone + 'static,
    Q2: Clone + 'static,
    A: Fn(&R1, &[T1]) -> Prop<R2, T2, C, O2, Q2> + Clone + 'static,
    OP: Fn(&O1) -> O2 + Clone + 'static,
    QP: Fn(&Q1) -> Q2 + Clone + 'static,
{
    match prop {
        Prop::Rel(r, ts) => terp_atomic(r, ts),
        Prop::Not(p) => Prop::Not(Box::new(terp_prop(terp_atomic, terp_op, terp_q, p))),
        Prop::Connected(c, p1, p2) => Prop::Connected(
            *c,
            Box::new(terp_prop(terp_atomic.clone(), terp_op.clone(), terp_q.clone(), p1)),
            Box::new(terp_prop(terp_atomic, terp_op, terp_q, p2)),
        ),
        Prop::NonLogConnected(c, p1, p2) => Prop::NonLogConnected(
            c.clone(),
            Box::new(terp_prop(terp_atomic.clone(), terp_op.clone(), terp_q.clone(), p1)),
            Box::new(terp_prop(terp_atomic, terp_op, terp_q, p2)),
        ),
        Prop::Modal(o, p) => Prop::Modal(
            terp_op(o),
            Box::new(terp_prop(terp_atomic, terp_op, terp_q, p)),
        ),
        Prop::Quantified(q, restriction, body) => {
            let mapped_restriction = restriction.clone().map(|r| {
                let terp_atomic = terp_atomic.clone();
                let terp_op = terp_op.clone();
                let terp_q = terp_q.clone();
                Rc::new(move |v| terp_prop(terp_atomic.clone(), terp_op.clone(), terp_q.clone(), &r(v))) as Rc<dyn Fn(i32) -> Prop<R2, T2, C, O2, Q2>>
            });
            let body = body.clone();
            Prop::Quantified(
                terp_q(q),
                mapped_restriction,
                Rc::new(move |v| terp_prop(terp_atomic.clone(), terp_op.clone(), terp_q.clone(), &body(v))),
            )
        }
        Prop::Eet => Prop::Eet,
    }
}
