//! Abstract syntax from [JboSyntax.hs](../JboSyntax.hs) (parse tree). Selbri/tag cycles use `Box`.

#![allow(non_camel_case_types)]

pub type Cmavo = String;
pub type Joik = String;
pub type Gadri = String;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Text {
    pub text_frees: Vec<Free>,
    pub vaguely_negated_text: bool,
    pub text_paras: Vec<Paragraph>,
}

pub type Paragraph = Vec<FragmentOrStatement>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FragmentOrStatement {
    Fragment(Fragment),
    Statement(Statement),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Fragment {
    /// `la NAME` sumti fragment (Haskell shows `[Fragment: "name"]` + `la name.`).
    FragLaName(String),
    FragPrenex(Vec<Term>),
    FragTerms(Vec<Term>),
    FragCon(Connective),
    FragQuantifier(Mex),
    FragNA(Cmavo),
    FragRels(Vec<RelClause>),
    FragLinks(Vec<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Statement {
    pub frees: Vec<Free>,
    pub prenex: Vec<Term>,
    pub body: Statement1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LogJboConnective {
    pub b1: bool,
    pub c: char,
    pub b2: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement1 {
    ConnectedStatement(Connective, Box<Statement1>, Box<Statement1>),
    StatementSentence {
        frees: Vec<Free>,
        sentence: Sentence,
    },
    StatementParas {
        tag: Option<Tag>,
        paras: Vec<Paragraph>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Subsentence {
    pub frees: Vec<Free>,
    pub prenex: Vec<Term>,
    pub sentence: Box<Sentence>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Sentence {
    pub terms: Vec<Term>,
    pub tail: Box<BridiTail>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Free {
    Bracketed(Box<Text>),
    Discursive(BridiTail),
    TruthQ(Option<i32>),
    Vocative(Vec<COI>, Option<Sumti>),
    MAI(Mex),
    XI(Mex),
    MexPrecedence(BridiTail),
    SOI(Sumti, Option<Sumti>),
    Indicator {
        indicator_nai: bool,
        indicator_cmavo: Cmavo,
    },
    NullFree,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct COI {
    pub coi_coi: String,
    pub coi_nai: bool,
}

pub type FreeIndex = i32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Sumti(Tagged, Sumti),
    Negation,
    Termset(Vec<Term>),
    ConnectedTerms(bool, Connective, Box<Term>, Box<Term>),
    BareTag(Tag),
    BareFA(Option<i32>),
    NullTerm,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Tagged {
    Untagged,
    Tagged(Tag),
    FATagged(i32),
    FAITagged,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AbsTag<R, T> {
    DecoratedTagUnits(Vec<DecoratedAbsTagUnit<R, T>>),
    ConnectedTag(
        AbsConnective<R, T>,
        Box<AbsTag<R, T>>,
        Box<AbsTag<R, T>>,
    ),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DecoratedAbsTagUnit<R, T> {
    pub tag_nahe: Option<Cmavo>,
    pub tag_se: Option<i32>,
    pub tag_nai: bool,
    pub tag_unit: AbsTagUnit<R, T>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AbsTagUnit<R, T> {
    TenseCmavo(Cmavo),
    CAhA(Cmavo),
    FAhA {
        faha_has_mohi: bool,
        faha_cmavo: Cmavo,
    },
    ROI {
        roiroi: Cmavo,
        roi_is_space: bool,
        roi_quantifier: AbsMex<R, T>,
    },
    TAhE_ZAhO {
        tahe_zohe_is_space: bool,
        tahe_zaho_cmavo: Cmavo,
    },
    BAI(Cmavo),
    FIhO(Box<R>),
    CUhE(Cmavo),
    KI,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AbsConnective<R, T> {
    JboConnLog(Option<Box<AbsTag<R, T>>>, LogJboConnective),
    JboConnJoik(Option<Box<AbsTag<R, T>>>, Joik),
}

pub type Connective = AbsConnective<Selbri, Sumti>;
pub type Tag = AbsTag<Selbri, Sumti>;
pub type DecoratedTagUnit = DecoratedAbsTagUnit<Selbri, Sumti>;
pub type TagUnit = AbsTagUnit<Selbri, Sumti>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Sumti {
    ConnectedSumti(
        bool,
        Connective,
        Box<Sumti>,
        Box<Sumti>,
        Vec<RelClause>,
    ),
    QAtom {
        frees: Vec<Free>,
        quant: Option<Mex>,
        rels: Vec<RelClause>,
        atom: Box<SumtiAtom>,
    },
    QSelbri {
        quant: Mex,
        rels: Vec<RelClause>,
        selbri: Selbri,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RelClause {
    Restrictive(Subsentence),
    Incidental(Subsentence),
    Descriptive(Subsentence),
    Assignment(Term),
    RestrictiveGOI(String, Term),
    IncidentalGOI(String, Term),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SumtiAtom {
    Name(Gadri, Vec<RelClause>, String),
    Variable(i32),
    NonAnaphoricProsumti(String),
    RelVar(i32),
    LambdaVar(Option<i32>, Option<i32>),
    SelbriVar,
    Description(
        Gadri,
        Option<Box<Sumti>>,
        Option<Mex>,
        EitherSelbriSumti,
        Vec<RelClause>,
        Vec<RelClause>,
    ),
    Assignable(i32),
    LerfuString(Vec<Lerfu>),
    Ri(i32),
    Ra(Cmavo),
    MainBridiSumbasti(i32),
    Quote(Box<Text>),
    NonJboQuote(String),
    ErrorQuote(Vec<String>),
    Word(String),
    MexLi(Mex),
    MexMex(Mex),
    Zohe,
    SumtiQ(Option<i32>),
    QualifiedSumti(SumtiQualifier, Vec<RelClause>, Box<Sumti>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EitherSelbriSumti {
    Selbri(Box<Selbri>),
    Sumti(Box<Sumti>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lerfu {
    LerfuChar(char),
    LerfuPA(Cmavo),
    LerfuValsi(String),
    LerfuShift(Cmavo),
    LerfuShifted(Cmavo, Box<Lerfu>),
    LerfuComposite(Vec<Lerfu>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SumtiQualifier {
    LAhE(String),
    NAhE_BO(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BridiTail {
    ConnectedBT(Connective, Box<BridiTail>, Box<BridiTail>, Vec<Term>),
    BridiTail3(Selbri, Vec<Term>),
    GekSentence(Box<GekSentence>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GekSentence {
    ConnectedGS(Connective, Box<Subsentence>, Box<Subsentence>, Vec<Term>),
    TaggedGS(Tag, Box<GekSentence>),
    NegatedGS(Box<GekSentence>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Selbri {
    Negated(Box<Selbri>),
    TaggedSelbri(Box<Tag>, Box<Selbri>),
    Selbri2(Selbri2),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Selbri2 {
    SBInverted(Selbri3, Box<Selbri2>),
    Selbri3(Selbri3),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Selbri3 {
    SBTanru(Box<Selbri3>, Box<Selbri3>),
    ConnectedSB(bool, Connective, Box<Selbri>, Box<Selbri3>),
    BridiBinding(Box<Selbri3>, Box<Selbri3>),
    ScalarNegatedSB(NAhE, Box<Selbri3>),
    /// Head — tanru unit with prefix frees / terms (camxes `TanruUnit` with subunits).
    TanruHead(Vec<Free>, Box<TanruUnit>, Vec<Term>),
}

pub type NAhE = Cmavo;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TanruUnit {
    TUBrivla(String),
    TUZei(Vec<String>),
    TUBridiQ(Option<i32>),
    TUGOhA(String, i32),
    TUMe(Box<Sumti>),
    TUMoi(Box<Sumti>, String),
    TUAbstraction(Abstractor, Subsentence),
    TUPermuted(i32, Box<TanruUnit>),
    TUJai(Option<Tag>, Box<TanruUnit>),
    TUOperator(Operator),
    TUXOhI(Tag),
    TUSelbri3(Selbri3),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Abstractor {
    NU(Cmavo),
    NegatedAbstractor(Box<Abstractor>),
    LogConnectedAbstractor(LogJboConnective, Box<Abstractor>, Box<Abstractor>),
    JoiConnectedAbstractor(Joik, Box<Abstractor>, Box<Abstractor>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AbsMex<R, T> {
    Operation(Box<AbsOperator<R, T>>, Vec<AbsMex<R, T>>),
    ConnectedMex(bool, AbsConnective<R, T>, Box<AbsMex<R, T>>, Box<AbsMex<R, T>>),
    QualifiedMex(SumtiQualifier, Box<AbsMex<R, T>>),
    MexInt(i32),
    MexNumeralString(Vec<Numeral>),
    MexLerfuString(Vec<Lerfu>),
    MexSelbri(Box<R>),
    MexSumti(Box<T>),
    MexArray(Vec<AbsMex<R, T>>),
}

pub type Mex = AbsMex<Selbri, Sumti>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Numeral {
    PA(Cmavo),
    NumeralLerfu(Lerfu),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AbsOperator<R, T> {
    ConnectedOperator(
        bool,
        AbsConnective<R, T>,
        Box<AbsOperator<R, T>>,
        Box<AbsOperator<R, T>>,
    ),
    OpPermuted(i32, Box<AbsOperator<R, T>>),
    OpScalarNegated(NAhE, Box<AbsOperator<R, T>>),
    OpMex(Box<AbsMex<R, T>>),
    OpSelbri(Box<R>),
    OpVUhU(Cmavo),
}

pub type Operator = AbsOperator<Selbri, Sumti>;

// ============================================================================
// Helper functions from JboSyntax.hs
// ============================================================================

// Ported from: JboSyntax.hs :: sb3tosb
/// Convert Selbri3 to Selbri by wrapping in Selbri2
pub fn sb3_to_sb(sb3: Selbri3) -> Selbri {
    Selbri::Selbri2(Selbri2::Selbri3(sb3))
}

// Ported from: JboSyntax.hs :: getsRi
/// Check if a SumtiAtom gets ri (anaphoric reference)
pub fn gets_ri(sa: &SumtiAtom) -> bool {
    match sa {
        SumtiAtom::Zohe => false,
        SumtiAtom::Assignable(_) => false,
        SumtiAtom::LerfuString(_) => false,
        SumtiAtom::MainBridiSumbasti(_) => false,
        SumtiAtom::Variable(_) => false,
        SumtiAtom::NonAnaphoricProsumti(p) => {
            matches!(p.as_str(), "ti" | "ta" | "tu")
        }
        _ => true,
    }
}

// Ported from: JboSyntax.hs :: lerfuStringsOfSelbri
/// Extract lerfu strings from a selbri for anaphora
pub fn lerfu_strings_of_selbri(selbri: &Selbri) -> Vec<Vec<Lerfu>> {
    match selbri {
        Selbri::Negated(sb) => lerfu_strings_of_selbri(sb),
        Selbri::TaggedSelbri(_, sb) => lerfu_strings_of_selbri(sb),
        Selbri::Selbri2(sb2) => {
            let mut result = Vec::new();
            for s in sb2_to_ls(sb2) {
                result.push(s.clone());
                result.push(s.iter().take(1).cloned().collect());
            }
            result
        }
    }
}

fn sb2_to_ls(sb2: &Selbri2) -> Vec<Vec<Lerfu>> {
    match sb2 {
        Selbri2::SBInverted(sb3, sb2_inner) => {
            let mut result = sb3_to_ls(sb3);
            result.extend(sb2_to_ls(sb2_inner));
            result
        }
        Selbri2::Selbri3(sb3) => sb3_to_ls(sb3),
    }
}

fn sb3_to_ls(sb3: &Selbri3) -> Vec<Vec<Lerfu>> {
    match sb3 {
        Selbri3::SBTanru(sb1, sb2) => {
            let mut result = sb3_to_ls(sb1);
            result.extend(sb3_to_ls(sb2));
            result
        }
        Selbri3::ConnectedSB(_, _, sb, sb3) => {
            let mut result = lerfu_strings_of_selbri(sb);
            result.extend(sb3_to_ls(sb3));
            result
        }
        Selbri3::BridiBinding(sb3, _) => sb3_to_ls(sb3),
        Selbri3::ScalarNegatedSB(_, sb3) => sb3_to_ls(sb3),
        Selbri3::TanruHead(_, tu, _) => tu_to_ls(tu),
    }
}

fn tu_to_ls(tu: &TanruUnit) -> Vec<Vec<Lerfu>> {
    match tu {
        TanruUnit::TUBrivla(s) => {
            if let Some(first_char) = s.chars().next() {
                vec![vec![Lerfu::LerfuChar(first_char)]]
            } else {
                vec![]
            }
        }
        TanruUnit::TUZei(vs) => {
            let full: Vec<Lerfu> = vs.iter()
                .filter_map(|v| v.chars().next())
                .map(Lerfu::LerfuChar)
                .collect();
            let first = vs.first()
                .and_then(|v| v.chars().next())
                .map(|c| vec![Lerfu::LerfuChar(c)])
                .unwrap_or_default();
            vec![full, first]
        }
        TanruUnit::TUAbstraction(abs, _) => {
            if let Abstractor::NU(s) = abs {
                if let Some(first_char) = s.chars().next() {
                    vec![
                        vec![Lerfu::LerfuChar(first_char)],
                        vec![Lerfu::LerfuValsi(s.clone())],
                    ]
                } else {
                    vec![]
                }
            } else {
                vec![]
            }
        }
        TanruUnit::TUSelbri3(sb3) => sb3_to_ls(sb3),
        _ => vec![],
    }
}

// Ported from: JboSyntax.hs :: tagNaiIsScalar
/// Check if a tag unit's NAI is scalar (ROI, TAhE_ZAhO, CAhA)
pub fn tag_nai_is_scalar<R, T>(tu: &AbsTagUnit<R, T>) -> bool {
    matches!(
        tu,
        AbsTagUnit::ROI { .. } | AbsTagUnit::TAhE_ZAhO { .. } | AbsTagUnit::CAhA(_)
    )
}

// Ported from: JboSyntax.hs :: isTense
/// Check if a tag is a tense tag (not BAI or FIhO)
pub fn is_tense<R, T>(tag: &AbsTag<R, T>) -> bool {
    match tag {
        AbsTag::ConnectedTag(_, t1, t2) => is_tense(t1) || is_tense(t2),
        AbsTag::DecoratedTagUnits(dtus) => {
            dtus.iter().any(|dtu| is_tense_dtu(&dtu.tag_unit))
        }
    }
}

fn is_tense_dtu<R, T>(tu: &AbsTagUnit<R, T>) -> bool {
    match tu {
        AbsTagUnit::BAI(_) => false,
        AbsTagUnit::FIhO(_) => false,
        _ => true,
    }
}
