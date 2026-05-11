//! AST mirrored from [`pappy/pappy/Pappy.hs`](../../../pappy/pappy/Pappy.hs).

pub type Identifier = String;
pub type RawCode = String;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Match {
    MatchAnon(Rule),
    MatchName(Rule, Identifier),
    MatchPat(Rule, RawCode),
    MatchString(Rule, String),
    MatchAnd(Rule),
    MatchNot(Rule),
    MatchPred(RawCode),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Producer {
    ProdName(Identifier),
    ProdCode(RawCode),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Rule {
    RulePrim(Identifier),
    RulePos,
    RuleChar(char),
    RuleSeq(Vec<Match>, Producer),
    RuleAlt(Vec<Rule>),
    RuleOpt(Box<Rule>),
    RuleError(Box<Rule>, String),
    RuleString(String),
    RuleStar(Box<Rule>),
    RulePlus(Box<Rule>),
    RuleExpect(Box<Rule>, Vec<String>),
    RuleSwitchChar(Vec<(char, Rule)>, Option<Box<Rule>>),
    RuleSwitchString(Identifier, Vec<(String, Rule)>, Option<Box<Rule>>),
}

pub type Nonterminal = (Identifier, RawCode, Rule);

#[derive(Clone, Debug)]
pub struct Grammar {
    pub grammar_name: Identifier,
    pub grammar_raw_code: RawCode,
    pub grammar_token: Option<Identifier>,
    pub grammar_imports: Vec<String>,
    pub grammar_tops: Vec<Identifier>,
    pub grammar_exports: Option<Vec<Identifier>>,
    pub grammar_nonterminals: Vec<Nonterminal>,
}
