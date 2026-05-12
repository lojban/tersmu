//! Semantic actions: fold a packrat [`ParseNode`](crate::camxes::peg::parsing::ParseNode) tree into a
//! [`SemanticNode`] tree with optional type-erased values per non-terminal.
//!
//! This runs **after** [`Peg::parse`](crate::camxes::peg::grammar::Peg::parse), so packrat memoization
//! stays unchanged; reducers see already-folded children (bottom-up order). This matches how a
//! hand-written attribute pass would sit on top of a PEG parse and is a practical stepping stone
//! toward richer “Pappy-style” per-production values in a future tersmu pipeline.

use crate::camxes::peg::grammar::Peg;
use crate::camxes::peg::parsing::{ParseError, ParseNode, ParseResult, Span};
use std::any::Any;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

/// Type-erased semantic value (typically `Arc<MyAst>`).
pub type DynVal = Arc<dyn Any + Send + Sync>;

/// Reducer for a named non-terminal: `(rule_name, span, folded_children, full_input)`.
pub type SemanticReducer = Arc<dyn Fn(&str, Span, &[SemanticNode], &str) -> Option<DynVal> + Send + Sync>;

/// Map of non-terminal name → reducer. Only listed rules get a [`SemanticNode::NonTerminal::value`].
#[derive(Clone, Default)]
pub struct ReducerTable {
    map: HashMap<String, SemanticReducer>,
}

impl ReducerTable {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, rule: impl Into<String>, reducer: SemanticReducer) -> Option<SemanticReducer> {
        self.map.insert(rule.into(), reducer)
    }

    /// Register a reducer that returns an optional concrete type; stored as [`DynVal`].
    pub fn on<T, F>(&mut self, rule: impl Into<String>, f: F)
    where
        T: Any + Send + Sync,
        F: Fn(&str, Span, &[SemanticNode], &str) -> Option<T> + Send + Sync + 'static,
    {
        let rule = rule.into();
        self.map.insert(
            rule,
            Arc::new(move |_name: &str, _span: Span, children: &[SemanticNode], input: &str| {
                f(_name, _span, children, input).map(|t| Arc::new(t) as DynVal)
            }),
        );
    }

    pub fn contains(&self, rule: &str) -> bool {
        self.map.contains_key(rule)
    }
}

/// Result of applying semantic actions: mirrors [`ParseNode`] with optional `value` on non-terminals.
#[derive(Clone)]
pub enum SemanticNode {
    Terminal { span: Span },
    NonTerminal {
        name: String,
        span: Span,
        children: Vec<SemanticNode>,
        /// Set when [`ReducerTable`] has an entry for this rule name.
        value: Option<DynVal>,
    },
}

impl fmt::Debug for SemanticNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemanticNode::Terminal { span } => f.debug_struct("Terminal").field("span", span).finish(),
            SemanticNode::NonTerminal {
                name,
                span,
                children,
                value,
            } => {
                let mut ds = f.debug_struct("NonTerminal");
                ds.field("name", name).field("span", span).field("children", children);
                match value {
                    None => ds.field("value", &None::<()>),
                    Some(_) => ds.field("value", &"Some(dyn Any)"),
                }
                    .finish()
            }
        }
    }
}

impl SemanticNode {
    pub fn span(&self) -> Span {
        match self {
            SemanticNode::Terminal { span } => *span,
            SemanticNode::NonTerminal { span, .. } => *span,
        }
    }

    /// `Some` when this node is [`SemanticNode::NonTerminal`] and carries a reduced value.
    pub fn value_ref(&self) -> Option<&DynVal> {
        match self {
            SemanticNode::Terminal { .. } => None,
            SemanticNode::NonTerminal { value, .. } => value.as_ref(),
        }
    }
}

/// Slice of `input` covered by `span` (byte offsets).
pub fn span_slice(input: &str, span: Span) -> &str {
    let Span(lo, hi) = span;
    if lo <= hi && hi <= input.len() {
        &input[lo..hi]
    } else {
        ""
    }
}

/// Downcast a [`DynVal`] to a concrete reference.
pub fn downcast_ref<T: Any>(v: &DynVal) -> Option<&T> {
    v.downcast_ref()
}

fn fold_node(node: &ParseNode, input: &str, reducers: &ReducerTable) -> SemanticNode {
    match node {
        ParseNode::Terminal { span } => SemanticNode::Terminal { span: *span },
        ParseNode::NonTerminal {
            name,
            span,
            children,
        } => {
            let sem_children: Vec<SemanticNode> = children.iter().map(|c| fold_node(c, input, reducers)).collect();
            let value = reducers
                .map
                .get(name)
                .and_then(|r| r(name.as_str(), *span, &sem_children, input));
            SemanticNode::NonTerminal {
                name: name.clone(),
                span: *span,
                children: sem_children,
                value,
            }
        }
    }
}

/// Fold every root [`ParseNode`] from a successful parse.
pub fn fold_parse_forest(nodes: &[ParseNode], input: &str, reducers: &ReducerTable) -> Vec<SemanticNode> {
    nodes.iter().map(|n| fold_node(n, input, reducers)).collect()
}

/// Parse with [`Peg::parse`], then apply semantic reducers (bottom-up).
pub fn parse_with_semantics(peg: &Peg, input: &str, reducers: &ReducerTable) -> Result<Vec<SemanticNode>, ParseError> {
    let ParseResult(_, _, _, payload) = peg.parse(input);
    match payload.as_ref() {
        Ok(nodes) => Ok(fold_parse_forest(nodes, input, reducers)),
        Err(e) => Err(e.clone()),
    }
}

/// If the parse yields exactly one root, return it.
pub fn single_root(forest: Vec<SemanticNode>) -> Option<SemanticNode> {
    match forest.len() {
        1 => Some(forest.into_iter().next().unwrap()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::camxes::peg::grammar::Peg;

    #[derive(Debug, PartialEq, Eq)]
    struct Bridi {
        head: String,
        pred: String,
    }

    #[test]
    fn reducer_builds_toy_bridi_ast() {
        // Toy pattern: sumti / selbri / bridi — analogous to tersmu building JboSyntax from camxes-shaped PEG.
        let grammar = r#"
        sentence <- sumti sp selbri
        sumti <- [a-z]+
        selbri <- [a-z]+
        sp <- [ ]+
        "#;
        let peg = Peg::new("sentence", grammar).expect("grammar");

        let mut table = ReducerTable::new();
        table.on("sumti", |_n, span, _ch, input| Some(span_slice(input, span).to_string()));
        table.on("selbri", |_n, span, _ch, input| Some(span_slice(input, span).to_string()));
        table.on("sentence", |_n, _span, ch, _input| {
            if ch.len() != 3 {
                return None;
            }
            let head = downcast_ref::<String>(ch[0].value_ref()?)?;
            let pred = downcast_ref::<String>(ch[2].value_ref()?)?;
            Some(Bridi {
                head: head.clone(),
                pred: pred.clone(),
            })
        });

        let forest = parse_with_semantics(&peg, "zarci klama", &table).expect("parse");
        let root = single_root(forest).expect("one root");
        let v = root.value_ref().expect("sentence value");
        let bridi = downcast_ref::<Bridi>(v).expect("Bridi");
        assert_eq!(
            bridi,
            &Bridi {
                head: "zarci".into(),
                pred: "klama".into(),
            }
        );
    }

    /// Morphology-flavoured: repeated syllable chunks fold into a `Word` sumti.
    #[test]
    fn morphology_style_syllable_word() {
        let grammar = r#"
        word <- syllable+
        syllable <- [a-z] [a-z] [a-z]
        "#;
        let peg = Peg::new("word", grammar).expect("grammar");
        let mut table = ReducerTable::new();
        table.on("syllable", |_n, span, _ch, input| Some(span_slice(input, span).to_string()));
        table.on("word", |_n, _span, ch, _input| {
            let mut s = String::new();
            for c in ch {
                let part = downcast_ref::<String>(c.value_ref()?)?;
                s.push_str(part.as_str());
            }
            Some(s)
        });
        // Nine letters → three CVV-style syllables (length divisible by 3 for this toy rule).
        let forest = parse_with_semantics(&peg, "bobybybob", &table).expect("parse");
        let root = single_root(forest).expect("root");
        let w = downcast_ref::<String>(root.value_ref().expect("v")).unwrap();
        assert_eq!(w, "bobybybob");
    }

    #[test]
    fn parse_error_skips_reducers() {
        let peg = Peg::new("start", "start <- 'ok'").unwrap();
        let mut table = ReducerTable::new();
        table.on("start", |_, _, _, _| Some(42u32));

        let err = parse_with_semantics(&peg, "nope", &table).unwrap_err();
        assert!(err.position <= 1);
    }

    #[test]
    fn rules_without_reducers_have_no_value() {
        let peg = Peg::new("a", "a <- 'x'").unwrap();
        let table = ReducerTable::new();
        let forest = parse_with_semantics(&peg, "x", &table).unwrap();
        let root = single_root(forest).unwrap();
        match root {
            SemanticNode::NonTerminal { value, .. } => assert!(value.is_none()),
            _ => panic!("expected NT"),
        }
    }
}
