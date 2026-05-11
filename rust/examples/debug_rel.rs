use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::peg::semantic::{parse_with_semantics, span_slice, ReducerTable, SemanticNode};

fn main() {
    let input = format!("{} %%%END%%%", std::env::args().skip(1).collect::<Vec<_>>().join(" "));
    let (_, grammar) = tersmu::camxes::LOJBAN_GRAMMAR;
    let peg = Peg::new("text", grammar).unwrap();
    let mut reducers = ReducerTable::new();
    for rule in ["subsentence", "sentence", "statement", "statement_1", "statement_2", "statement_3", "bridi_tail", "bridi_tail_1", "bridi_tail_2", "bridi_tail_3", "selbri", "selbri_1", "selbri_2", "selbri_3", "selbri_4", "selbri_5", "tanru_unit", "tanru_unit_1", "tanru_unit_2", "NUhA_clause", "operator", "operator_0", "operator_1", "operator_2", "mex_operator", "VUhU_clause", "li_clause", "mex", "mex_0", "mex_1", "mex_2", "operand", "operand_3", "number", "PA_clause", "relative_clause_1", "relative_clause", "relative_clauses", "sumti", "sumti_1", "sumti_2", "sumti_3", "sumti_4", "sumti_5", "sumti_6", "term", "term_1"] {
        reducers.on(rule, |name, span, children, input| {
            eprintln!("{} {:?}: {:?}", name, span, span_slice(input, span).trim());
            for child in children {
                if let SemanticNode::NonTerminal { name, span, value, .. } = child {
                    eprintln!("  child {} {:?} value={}", name, span, value.is_some());
                }
            }
            Some(format!("{}:{}", name, span_slice(input, span).trim()))
        });
    }
    let _ = parse_with_semantics(&peg, &input, &reducers).unwrap();
}
