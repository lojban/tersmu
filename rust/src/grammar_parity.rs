//! Rule-name parity helpers between legacy `Lojban.pappy` and `camxes-rs` `lojban.peg`.
//!
//! We keep this mapping centralized so reducers can be written against stable semantic
//! concepts without re-deriving a new PEG from Pappy.

/// `(pappy_rule, camxes_peg_rule)` pairs used by reducer wiring.
pub const RULE_PARITY: &[(&str, &str)] = &[
    ("terminatedText", "text"),
    ("text", "text"),
    ("paragraph", "paragraph"),
    ("statement", "statement"),
    ("sentence", "sentence"),
    ("subsentence", "subsentence"),
    ("bridiTail", "bridi_tail"),
    ("term", "term"),
    ("sumti", "sumti"),
    ("selbri", "selbri"),
    ("tanruUnit", "tanru_unit"),
    ("free", "free"),
    ("tag", "tag"),
    ("quantifier", "quantifier"),
    ("mex", "mex"),
    ("operator", "operator"),
];

/// Prefer the camxes PEG name for a given legacy Pappy rule.
pub fn camxes_rule_name(pappy_rule: &str) -> &str {
    RULE_PARITY
        .iter()
        .find_map(|(p, c)| (*p == pappy_rule).then_some(*c))
        .unwrap_or(pappy_rule)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_rules_map_to_camxes_names() {
        assert_eq!(camxes_rule_name("tanruUnit"), "tanru_unit");
        assert_eq!(camxes_rule_name("terminatedText"), "text");
    }

    #[test]
    fn unknown_rules_fall_back_to_identity() {
        assert_eq!(camxes_rule_name("custom_rule"), "custom_rule");
    }
}
