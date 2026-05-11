//! Integration tests for semantic actions — patterns aimed at a future tersmu Rust pipeline:
//! parse → [`tersmu::camxes::peg::SemanticNode`] forest with per-rule reducers → downcast to app AST.

use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::peg::{
    downcast_ref, parse_with_semantics, single_root, span_slice, ReducerTable, SemanticNode,
};

#[derive(Debug, PartialEq, Eq)]
struct PreparsedChunk {
    /// Simulates stripping / normalizing before main parse (cf. morph + `%%%END%%%`).
    body: String,
}

#[test]
fn end_marker_strip_then_statement_ast() {
    let grammar = r#"
    text <- chunk endmark
    chunk <- [a-z ]+
    endmark <- "%%%END%%%"
    "#;
    let peg = Peg::new("text", grammar).expect("peg");

    let mut table = ReducerTable::new();
    table.on("chunk", |_n, span, _ch, input| {
        Some(PreparsedChunk {
            body: span_slice(input, span).trim().to_string(),
        })
    });

    let forest = parse_with_semantics(&peg, "mi klama le zarci %%%END%%%", &table).expect("ok");
    let root = single_root(forest).expect("one root");
    let SemanticNode::NonTerminal { children, .. } = &root else {
        panic!("expected NT");
    };
    let chunk_node = children.iter().find(|c| matches!(c, SemanticNode::NonTerminal { name, .. } if name == "chunk")).expect("chunk child");
    let v = chunk_node.value_ref().expect("chunk reduced");
    let chunk = downcast_ref::<PreparsedChunk>(v).expect("type");
    assert_eq!(chunk.body, "mi klama le zarci");
}

#[test]
fn tanru_style_predicate_then_sumti() {
    // Spaced selbri + sumti (like many tersmu productions that use explicit `sp`).
    let grammar = r#"
    bridi <- predicate sp sumti
    predicate <- [a-z]+
    sp <- [ ]+
    sumti <- [a-z]+
    "#;
    let peg = Peg::new("bridi", grammar).expect("peg");
    let mut table = ReducerTable::new();
    table.on("predicate", |_n, span, _, input| Some(span_slice(input, span).to_string()));
    table.on("sumti", |_n, span, _, input| Some(span_slice(input, span).to_string()));
    table.on("bridi", |_n, _s, ch, _| {
        let pred = downcast_ref::<String>(ch[0].value_ref()?)?;
        let obj = downcast_ref::<String>(ch[2].value_ref()?)?;
        Some((pred.clone(), Some(obj.clone())))
    });

    let forest = parse_with_semantics(&peg, "klama zarci", &table).unwrap();
    let r = single_root(forest).unwrap();
    let tuple = downcast_ref::<(String, Option<String>)>(r.value_ref().unwrap()).unwrap();
    assert_eq!(tuple.0, "klama");
    assert_eq!(tuple.1, Some("zarci".into()));
}

#[test]
fn optional_tail_matches_tanru_unit_pattern() {
    let grammar = r#"
    bridi <- predicate opt_tail?
    opt_tail <- sp sumti
    predicate <- [a-z]+
    sp <- [ ]+
    sumti <- [a-z]+
    "#;
    let peg = Peg::new("bridi", grammar).expect("peg");
    let mut table = ReducerTable::new();
    table.on("predicate", |_n, span, _, input| Some(span_slice(input, span).to_string()));
    table.on("sumti", |_n, span, _, input| Some(span_slice(input, span).to_string()));
    table.on("opt_tail", |_n, _s, ch, _| {
        let s = downcast_ref::<String>(ch[1].value_ref()?)?;
        Some(s.clone())
    });
    table.on("bridi", |_n, _s, ch, _| {
        let pred = downcast_ref::<String>(ch[0].value_ref()?)?;
        let extra = ch.get(1).and_then(|n| n.value_ref()).and_then(|v| downcast_ref::<String>(v).cloned());
        Some((pred.clone(), extra))
    });

    let with_tail = parse_with_semantics(&peg, "klama zarci", &table).unwrap();
    let root = single_root(with_tail).unwrap();
    let t = downcast_ref::<(String, Option<String>)>(root.value_ref().unwrap()).unwrap();
    assert_eq!(t.0, "klama");
    assert_eq!(t.1.as_deref(), Some("zarci"));

    let bare = parse_with_semantics(&peg, "klama", &table).unwrap();
    let root2 = single_root(bare).unwrap();
    let t2 = downcast_ref::<(String, Option<String>)>(root2.value_ref().unwrap()).unwrap();
    assert_eq!(t2.0, "klama");
    assert_eq!(t2.1, None);
}

#[test]
fn nudge_frees_error_position_is_byte_offset() {
    let peg = Peg::new("t", "t <- 'a'").unwrap();
    let table = ReducerTable::new();
    let err = parse_with_semantics(&peg, "€a", &table).unwrap_err();
    // Invalid UTF-8 boundary: still reports a position usable with line_column (tersmu-style preparsing).
    assert!(err.position <= 4);
}

#[test]
fn forest_shape_matches_parse_roots() {
    let peg = Peg::new("s", "s <- 'x'").unwrap();
    let table = ReducerTable::new();
    let v = parse_with_semantics(&peg, "x", &table).unwrap();
    assert_eq!(v.len(), 1);
    assert!(matches!(v[0], SemanticNode::NonTerminal { .. }));
}
