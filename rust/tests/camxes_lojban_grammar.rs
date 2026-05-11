use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::LOJBAN_GRAMMAR;

fn lojban_parser() -> Peg {
    let (start, grammar) = LOJBAN_GRAMMAR;
    Peg::new(start, grammar).expect("LOJBAN_GRAMMAR should compile")
}

#[test]
fn lojban_grammar_builds() {
    let _ = lojban_parser();
}

#[test]
fn parses_simple_bridi() {
    let peg = lojban_parser();
    let input = "mi klama le zarci";
    let result = peg.parse(input);
    let nodes = result.3.as_ref().as_ref().expect("parse success");
    assert!(!nodes.is_empty());
    assert!(result.1 > 0);
    assert!(result.1 <= input.len());
}

#[test]
fn parses_multiple_sentence_shapes() {
    let peg = lojban_parser();
    let samples = [
        "mi prami do",
        "lo mlatu cu viska lo gerku",
        ".i mi citka",
        "coi rodo",
    ];
    for sample in samples {
        let result = peg.parse(sample);
        assert!(
            result.3.is_ok(),
            "expected parse success for '{sample}', got {:?}",
            result.3
        );
        assert!(result.1 > 0, "expected non-empty consumption for '{sample}'");
        assert!(result.1 <= sample.len(), "expected valid byte position for '{sample}'");
    }
}

#[test]
fn invalid_or_partial_input_has_sane_position() {
    let peg = lojban_parser();
    let input = "@@@@";
    let result = peg.parse(input);
    match result.3.as_ref().as_ref() {
        Ok(_) => {
            // This grammar may accept an empty/partial text; in that case we still require
            // the parser position to be a valid byte offset and detect non-full consumption.
            assert!(result.1 <= input.len());
            assert!(result.1 < input.len());
        }
        Err(err) => {
            assert!(err.position <= input.len());
        }
    }
}
