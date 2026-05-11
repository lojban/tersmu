use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::peg::semantic::{parse_with_semantics, span_slice, ReducerTable, SemanticNode};

fn main() {
    let input = format!("{} %%%END%%%", std::env::args().skip(1).collect::<Vec<_>>().join(" "));
    let (_, grammar) = tersmu::camxes::LOJBAN_GRAMMAR;
    let peg = Peg::new("text", grammar).unwrap();
    let mut reducers = ReducerTable::new();
    for rule in ["termset", "termsGikTerms", "terms", "terms_1", "terms_2", "term", "term_1", "sumti", "sumti_1", "sumti_2", "sumti_3", "sumti_4", "sumti_5", "gek_sentence", "gek", "gik", "tag", "stag", "tense_modal", "simple_tense_modal", "BAI_clause", "GI_clause", "subsentence", "sentence", "bridi_tail", "bridi_tail_3"] {
        reducers.on(rule, |name, span, children, input| {
            eprintln!("{} {:?}: {:?}", name, span, span_slice(input, span).trim());
            for child in children {
                if let SemanticNode::NonTerminal { name, span, value, .. } = child {
                    eprintln!("  child {} {:?} {:?} value={}", name, span, span_slice(input, *span).trim(), value.is_some());
                }
            }
            None::<String>
        });
    }
    let _ = parse_with_semantics(&peg, &input, &reducers).unwrap();
}
