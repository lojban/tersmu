//! Evaluation-layer types from [JboProp.hs](../JboProp.hs). Filled in as [crate::eval](super::eval) grows.

pub use crate::jbo_syntax::Fragment;
pub use crate::logic::Prop;
use std::sync::Arc;

/// JboTerm: Lojban terms (sumti) in the semantic representation
pub enum JboTerm {
    /// Bound variable (from quantifier)
    BoundVar(i32),
    /// Free variable
    Var(i32),
    /// Constant with index and optional arguments
    Constant(i32, Vec<JboTerm>),
    /// Named entity (la)
    Named(String),
    /// Predicate-based name (la with complex selbri)
    PredNamed(JboPred),
    /// Non-anaphoric prosumti (mi, do, ti, etc.)
    NonAnaph(String),
    /// Unfilled place
    Unfilled,
    /// Unbound sumbasti (anaphora resolution failed)
    UnboundSumbasti(crate::jbo_syntax::SumtiAtom),
    /// Mex value as term
    Value(JboMex),
    /// Qualified term (LAhE, NAhE_BO)
    QualifiedTerm(crate::jbo_syntax::SumtiQualifier, Box<JboTerm>),
    /// Mex as term
    TheMex(crate::jbo_syntax::Mex),
    /// Joiked terms (collective/mass)
    JoikedTerms(String, Box<JboTerm>, Box<JboTerm>),
    /// Jbo quote
    JboQuote(JboText),
    /// Error quote
    JboErrorQuote(Vec<String>),
    /// Non-Jbo quote
    JboNonJboQuote(String),
    /// Valsi (word)
    Valsi(String),
    /// Term with attached side texticules (SEI/ti'o)
    TermWithSides(Box<JboTerm>, Vec<Texticule>),
}

impl PartialEq for JboTerm {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (JboTerm::BoundVar(a), JboTerm::BoundVar(b)) => a == b,
            (JboTerm::Var(a), JboTerm::Var(b)) => a == b,
            (JboTerm::Constant(an, aa), JboTerm::Constant(bn, ba)) => an == bn && aa == ba,
            (JboTerm::Named(a), JboTerm::Named(b)) => a == b,
            (JboTerm::PredNamed(_), JboTerm::PredNamed(_)) => false, // Predicates not comparable
            (JboTerm::NonAnaph(a), JboTerm::NonAnaph(b)) => a == b,
            (JboTerm::Unfilled, JboTerm::Unfilled) => true,
            (JboTerm::UnboundSumbasti(a), JboTerm::UnboundSumbasti(b)) => a == b,
            (JboTerm::Value(a), JboTerm::Value(b)) => a == b,
            (JboTerm::QualifiedTerm(aq, at), JboTerm::QualifiedTerm(bq, bt)) => aq == bq && at == bt,
            (JboTerm::TheMex(a), JboTerm::TheMex(b)) => a == b,
            (JboTerm::JoikedTerms(aj, at1, at2), JboTerm::JoikedTerms(bj, bt1, bt2)) => {
                aj == bj && at1 == bt1 && at2 == bt2
            }
            (JboTerm::JboQuote(_), JboTerm::JboQuote(_)) => false,
            (JboTerm::JboErrorQuote(a), JboTerm::JboErrorQuote(b)) => a == b,
            (JboTerm::JboNonJboQuote(a), JboTerm::JboNonJboQuote(b)) => a == b,
            (JboTerm::Valsi(a), JboTerm::Valsi(b)) => a == b,
            (JboTerm::TermWithSides(a, asides), JboTerm::TermWithSides(b, bsides)) => a == b && asides.len() == bsides.len(),
            _ => false,
        }
    }
}

impl Eq for JboTerm {}

impl Clone for JboTerm {
    fn clone(&self) -> Self {
        match self {
            JboTerm::BoundVar(n) => JboTerm::BoundVar(*n),
            JboTerm::Var(n) => JboTerm::Var(*n),
            JboTerm::Constant(n, args) => JboTerm::Constant(*n, args.clone()),
            JboTerm::Named(s) => JboTerm::Named(s.clone()),
            JboTerm::PredNamed(pred) => JboTerm::PredNamed(pred.clone()),
            JboTerm::NonAnaph(s) => JboTerm::NonAnaph(s.clone()),
            JboTerm::Unfilled => JboTerm::Unfilled,
            JboTerm::UnboundSumbasti(sa) => JboTerm::UnboundSumbasti(sa.clone()),
            JboTerm::Value(m) => JboTerm::Value(m.clone()),
            JboTerm::QualifiedTerm(q, t) => JboTerm::QualifiedTerm(q.clone(), t.clone()),
            JboTerm::TheMex(m) => JboTerm::TheMex(m.clone()),
            JboTerm::JoikedTerms(j, t1, t2) => JboTerm::JoikedTerms(j.clone(), t1.clone(), t2.clone()),
            JboTerm::JboQuote(text) => JboTerm::JboQuote(text.clone()),
            JboTerm::JboErrorQuote(words) => JboTerm::JboErrorQuote(words.clone()),
            JboTerm::JboNonJboQuote(s) => JboTerm::JboNonJboQuote(s.clone()),
            JboTerm::Valsi(s) => JboTerm::Valsi(s.clone()),
            JboTerm::TermWithSides(t, sides) => JboTerm::TermWithSides(t.clone(), sides.clone()),
        }
    }
}

impl std::fmt::Debug for JboTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JboTerm::BoundVar(n) => f.debug_tuple("BoundVar").field(n).finish(),
            JboTerm::Var(n) => f.debug_tuple("Var").field(n).finish(),
            JboTerm::Constant(n, args) => f.debug_tuple("Constant").field(n).field(args).finish(),
            JboTerm::Named(s) => f.debug_tuple("Named").field(s).finish(),
            JboTerm::PredNamed(_) => write!(f, "PredNamed(<fn>)"),
            JboTerm::NonAnaph(s) => f.debug_tuple("NonAnaph").field(s).finish(),
            JboTerm::Unfilled => write!(f, "Unfilled"),
            JboTerm::UnboundSumbasti(sa) => f.debug_tuple("UnboundSumbasti").field(sa).finish(),
            JboTerm::Value(m) => f.debug_tuple("Value").field(m).finish(),
            JboTerm::QualifiedTerm(q, t) => f.debug_tuple("QualifiedTerm").field(q).field(t).finish(),
            JboTerm::TheMex(m) => f.debug_tuple("TheMex").field(m).finish(),
            JboTerm::JoikedTerms(j, t1, t2) => f.debug_tuple("JoikedTerms").field(j).field(t1).field(t2).finish(),
            JboTerm::JboQuote(text) => f.debug_tuple("JboQuote").field(text).finish(),
            JboTerm::JboErrorQuote(words) => f.debug_tuple("JboErrorQuote").field(words).finish(),
            JboTerm::JboNonJboQuote(s) => f.debug_tuple("JboNonJboQuote").field(s).finish(),
            JboTerm::Valsi(s) => f.debug_tuple("Valsi").field(s).finish(),
            JboTerm::TermWithSides(t, sides) => f.debug_tuple("TermWithSides").field(t).field(sides).finish(),
        }
    }
}

impl Clone for JboRel {
    fn clone(&self) -> Self {
        match self {
            JboRel::Brivla(s) => JboRel::Brivla(s.clone()),
            JboRel::Equal => JboRel::Equal,
            JboRel::Among(t) => JboRel::Among(t.clone()),
            JboRel::Tanru(r1, r2) => JboRel::Tanru(r1.clone(), r2.clone()),
            JboRel::AppliedRel(r, terms) => JboRel::AppliedRel(r.clone(), terms.clone()),
            JboRel::TanruConnective(con, p1, p2) => {
                JboRel::TanruConnective(con.clone(), p1.clone(), p2.clone())
            }
            JboRel::PermutedRel(n, r) => JboRel::PermutedRel(*n, r.clone()),
            JboRel::RVar(n) => JboRel::RVar(*n),
            JboRel::BoundRVar(n) => JboRel::BoundRVar(*n),
            JboRel::RAss(n) => JboRel::RAss(*n),
            JboRel::UnboundBribasti(tu) => JboRel::UnboundBribasti(tu.clone()),
            JboRel::Moi(t, s) => JboRel::Moi(t.clone(), s.clone()),
            JboRel::OperatorRel(op) => JboRel::OperatorRel(op.clone()),
            JboRel::ScalarNegatedRel(s, r) => JboRel::ScalarNegatedRel(s.clone(), r.clone()),
            JboRel::VPredRel(vpred) => JboRel::VPredRel(vpred.clone()),
            JboRel::AbsPred(a, p) => JboRel::AbsPred(a.clone(), p.clone()),
            JboRel::AbsProp(a, p) => JboRel::AbsProp(a.clone(), p.clone()),
            JboRel::TagRel(t) => JboRel::TagRel(t.clone()),
            JboRel::ModalRel(m, r) => JboRel::ModalRel(m.clone(), r.clone()),
        }
    }
}

impl std::fmt::Debug for JboRel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JboRel::Brivla(s) => f.debug_tuple("Brivla").field(s).finish(),
            JboRel::Equal => write!(f, "Equal"),
            JboRel::Among(t) => f.debug_tuple("Among").field(t).finish(),
            JboRel::Tanru(r1, r2) => f.debug_tuple("Tanru").field(r1).field(r2).finish(),
            JboRel::AppliedRel(r, terms) => f.debug_tuple("AppliedRel").field(r).field(terms).finish(),
            JboRel::TanruConnective(con, p1, p2) => f.debug_tuple("TanruConnective").field(con).field(p1).field(p2).finish(),
            JboRel::PermutedRel(n, r) => f.debug_tuple("PermutedRel").field(n).field(r).finish(),
            JboRel::RVar(n) => f.debug_tuple("RVar").field(n).finish(),
            JboRel::BoundRVar(n) => f.debug_tuple("BoundRVar").field(n).finish(),
            JboRel::RAss(n) => f.debug_tuple("RAss").field(n).finish(),
            JboRel::UnboundBribasti(tu) => f.debug_tuple("UnboundBribasti").field(tu).finish(),
            JboRel::Moi(t, s) => f.debug_tuple("Moi").field(t).field(s).finish(),
            JboRel::OperatorRel(op) => f.debug_tuple("OperatorRel").field(op).finish(),
            JboRel::ScalarNegatedRel(s, r) => f.debug_tuple("ScalarNegatedRel").field(s).field(r).finish(),
            JboRel::VPredRel(_) => write!(f, "VPredRel(<fn>)"),
            JboRel::AbsPred(abs, _) => write!(f, "AbsPred({}, <fn>)", abs),
            JboRel::AbsProp(abs, _) => write!(f, "AbsProp({}, <prop>)", abs),
            JboRel::TagRel(t) => f.debug_tuple("TagRel").field(t).finish(),
            JboRel::ModalRel(m, r) => f.debug_tuple("ModalRel").field(m).field(r).finish(),
        }
    }
}

impl PartialEq for JboRel {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (JboRel::Brivla(s1), JboRel::Brivla(s2)) => s1 == s2,
            (JboRel::Equal, JboRel::Equal) => true,
            (JboRel::Among(t1), JboRel::Among(t2)) => t1 == t2,
            (JboRel::Tanru(r1a, r1b), JboRel::Tanru(r2a, r2b)) => r1a == r2a && r1b == r2b,
            (JboRel::AppliedRel(r1, terms1), JboRel::AppliedRel(r2, terms2)) => r1 == r2 && terms1 == terms2,
            (JboRel::TanruConnective(c1, p1a, p1b), JboRel::TanruConnective(c2, p2a, p2b)) => c1 == c2 && p1a == p2a && p1b == p2b,
            (JboRel::PermutedRel(n1, r1), JboRel::PermutedRel(n2, r2)) => n1 == n2 && r1 == r2,
            (JboRel::RVar(n1), JboRel::RVar(n2)) => n1 == n2,
            (JboRel::BoundRVar(n1), JboRel::BoundRVar(n2)) => n1 == n2,
            (JboRel::UnboundBribasti(tu1), JboRel::UnboundBribasti(tu2)) => tu1 == tu2,
            (JboRel::Moi(t1, s1), JboRel::Moi(t2, s2)) => t1 == t2 && s1 == s2,
            (JboRel::OperatorRel(op1), JboRel::OperatorRel(op2)) => op1 == op2,
            (JboRel::ScalarNegatedRel(s1, r1), JboRel::ScalarNegatedRel(s2, r2)) => s1 == s2 && r1 == r2,
            (JboRel::VPredRel(_), JboRel::VPredRel(_)) => false, // Function pointers can't be compared
            (JboRel::AbsPred(_, _), JboRel::AbsPred(_, _)) => false, // Function pointers can't be compared
            (JboRel::AbsProp(_, _), JboRel::AbsProp(_, _)) => false,
            (JboRel::TagRel(_), JboRel::TagRel(_)) => false,
            (JboRel::ModalRel(_, _), JboRel::ModalRel(_, _)) => false,
            _ => false,
        }
    }
}

impl Eq for JboRel {}

/// JboRel: Lojban relations (selbri) in the semantic representation
pub enum JboRel {
    /// Simple brivla predicate
    Brivla(String),
    /// Equality relation (du)
    Equal,
    /// Among relation (for gadri)
    Among(Box<JboTerm>),
    /// Tanru (compound predicate)
    Tanru(Box<JboRel>, Box<JboRel>),
    /// Relation with already-filled places
    AppliedRel(Box<JboRel>, Vec<JboTerm>),
    /// Connected tanru predicates
    TanruConnective(crate::jbo_syntax::Connective, Box<JboRel>, Box<JboRel>),
    /// Place-converted relation (se/te/ve/xe)
    PermutedRel(i32, Box<JboRel>),
    /// Relation variable (unassigned)
    RVar(i32),
    /// Bound relation variable (unassigned, from bu'a selbri)
    BoundRVar(i32),
    /// Assigned relation variable (from cei definitions)
    RAss(i32),
    /// Unbound bribasti (relation resolution failed)
    UnboundBribasti(crate::jbo_syntax::TanruUnit),
    /// Moi relation (ordinal/cardinal)
    Moi(Box<JboTerm>, String),
    /// Operator as relation
    OperatorRel(Box<JboOperator>),
    /// Scalar negated relation (na'e, no'e, to'e)
    ScalarNegatedRel(String, Box<JboRel>),
    /// Variadic predicate as relation
    VPredRel(JboVPred),
    /// Lambda abstraction (ka/ni with fixed-arity predicate)
    AbsPred(String, JboNPred),
    /// Proposition abstraction (nu/du'u/etc.)
    AbsProp(String, Box<JboProp>),
    /// Modal tag used as a relation
    TagRel(JboTag),
    /// Modal proposition used as a tanru modifier
    ModalRel(JboModalOp, Box<JboRel>),
}

#[derive(Debug, Clone)]
pub enum JboModalOp {
    /// Non-veridical marker
    NonVeridical,
    /// Tagged modal with optional term
    Tagged(JboTag, Option<JboTerm>),
    /// With event as term
    WithEventAs(JboTerm),
    /// Question truth modal
    QTruthModal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JboQuantifier {
    /// Mex-based quantifier
    MexQuantifier(JboMex),
    /// Logical quantifier (exists, forall, exactly)
    LojQuantifier(crate::logic::LojQuantifier),
    /// Question quantifier (ma, mo)
    QuestionQuantifier,
    /// Relation quantifier wrapper
    RelQuantifier(Box<JboQuantifier>),
}

pub type JboProp = Prop<JboRel, JboTerm, String, JboModalOp, JboQuantifier>;

// Ported from: JboProp.hs :: JboTag, JboTagUnit, DecoratedTagUnit
/// Tag structure for modals and tenses
#[derive(Debug, Clone)]
pub enum JboTag {
    DecoratedTagUnits(Vec<DecoratedTagUnit>),
    ConnectedTag(Box<JboConnective>, Box<JboTag>, Box<JboTag>),
}

#[derive(Debug, Clone)]
pub struct DecoratedTagUnit {
    pub nahe: Option<String>,
    pub se: Option<i32>,
    pub nai: bool,
    pub tag_unit: JboTagUnit,
}

pub enum JboTagUnit {
    TenseCmavo(String),
    CAhA(String),
    BAI(String),
    FAhA(Option<bool>, String),
    #[allow(non_camel_case_types)]
    TAhE_ZAhO(bool, String),
    ROI(String, bool, JboMex),
    FIhO(JboVPred),
    KI,
    CUhE(String),
}

// Manual Debug implementation for JboTagUnit
impl std::fmt::Debug for JboTagUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JboTagUnit::TenseCmavo(s) => write!(f, "TenseCmavo({:?})", s),
            JboTagUnit::CAhA(s) => write!(f, "CAhA({:?})", s),
            JboTagUnit::BAI(s) => write!(f, "BAI({:?})", s),
            JboTagUnit::FAhA(m, s) => write!(f, "FAhA({:?}, {:?})", m, s),
            JboTagUnit::TAhE_ZAhO(b, s) => write!(f, "TAhE_ZAhO({:?}, {:?})", b, s),
            JboTagUnit::ROI(s, b, m) => write!(f, "ROI({:?}, {:?}, {:?})", s, b, m),
            JboTagUnit::FIhO(_) => write!(f, "FIhO(<vpred>)"),
            JboTagUnit::KI => write!(f, "KI"),
            JboTagUnit::CUhE(s) => write!(f, "CUhE({:?})", s),
        }
    }
}

// Manual Clone implementation for JboTagUnit
impl Clone for JboTagUnit {
    fn clone(&self) -> Self {
        match self {
            JboTagUnit::TenseCmavo(s) => JboTagUnit::TenseCmavo(s.clone()),
            JboTagUnit::CAhA(s) => JboTagUnit::CAhA(s.clone()),
            JboTagUnit::BAI(s) => JboTagUnit::BAI(s.clone()),
            JboTagUnit::FAhA(m, s) => JboTagUnit::FAhA(*m, s.clone()),
            JboTagUnit::TAhE_ZAhO(b, s) => JboTagUnit::TAhE_ZAhO(*b, s.clone()),
            JboTagUnit::ROI(s, b, m) => JboTagUnit::ROI(s.clone(), *b, m.clone()),
            JboTagUnit::FIhO(p) => JboTagUnit::FIhO(p.clone()),
            JboTagUnit::KI => JboTagUnit::KI,
            JboTagUnit::CUhE(s) => JboTagUnit::CUhE(s.clone()),
        }
    }
}

// Ported from: JboProp.hs :: JboConnective
/// Connective structure
#[derive(Debug, Clone)]
pub enum JboConnective {
    JboConnLog(Option<Box<JboTag>>, crate::jbo_syntax::LogJboConnective),
    JboConnJoik(Option<Box<JboTag>>, String),
}

#[derive(Debug, Clone)]
pub enum JboFragment {
    JboFragTerms(Vec<JboTerm>),
    JboFragUnparsed(Fragment),
}

pub type JboText = Vec<Texticule>;

#[derive(Debug, Clone)]
pub enum Texticule {
    TexticuleFrag(JboFragment),
    TexticuleProp(JboProp),
    TexticuleSide(SideType, Box<Texticule>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SideType {
    SideBracketed,
    SideDiscursive,
}

// ============================================================================
// Additional types from JboProp.hs
// ============================================================================

/// JboVPred: Variadic predicate (takes list of terms)
pub type JboVPred = Arc<dyn Fn(&[JboTerm]) -> JboProp + Send + Sync>;

/// JboNPred: N-ary predicate with fixed arity
#[derive(Clone)]
pub struct JboNPred {
    pub arity: usize,
    pub pred: JboNPredFn,
}

type JboNPredFn = Arc<dyn Fn(&[JboTerm]) -> JboProp + Send + Sync>;

/// JboPred: Unary predicate
pub type JboPred = Arc<dyn Fn(&JboTerm) -> JboProp + Send + Sync>;

// Ported from: JboProp.hs :: vPredToPred
/// Convert variadic predicate to unary predicate
pub fn v_pred_to_pred(vpred: JboVPred) -> JboPred {
    Arc::new(move |o: &JboTerm| vpred(std::slice::from_ref(o)))
}

// Ported from: JboProp.hs :: predToNPred
/// Convert unary predicate to n-ary predicate
pub fn pred_to_n_pred(pred: JboPred) -> JboNPred {
    JboNPred {
        arity: 1,
        pred: Arc::new(move |os: &[JboTerm]| {
            if let Some(o) = os.first() {
                pred(o)
            } else {
                Prop::Eet
            }
        }),
    }
}

// Ported from: JboProp.hs :: jboPredToLojPred
/// Convert JboPred to Logic Pred
pub fn jbo_pred_to_loj_pred(pred: &JboPred) -> std::sync::Arc<dyn Fn(i32) -> JboProp + Send + Sync> {
    let pred = pred.clone();
    std::sync::Arc::new(move |v: i32| pred(&JboTerm::BoundVar(v)))
}

// ============================================================================
// Mex and Operator types
// ============================================================================

/// JboMex: Mathematical expression in semantic representation
pub enum JboMex {
    Operation(Box<JboOperator>, Vec<JboMex>),
    ConnectedMex(bool, crate::jbo_syntax::Connective, Box<JboMex>, Box<JboMex>),
    QualifiedMex(crate::jbo_syntax::SumtiQualifier, Box<JboMex>),
    MexInt(i32),
    MexNumeralString(Vec<crate::jbo_syntax::Numeral>),
    MexLerfuString(Vec<crate::jbo_syntax::Lerfu>),
    MexSelbri(Box<JboVPred>),
    MexSumti(Box<JboTerm>),
    MexArray(Vec<JboMex>),
}

impl Clone for JboMex {
    fn clone(&self) -> Self {
        match self {
            JboMex::Operation(op, ms) => JboMex::Operation(op.clone(), ms.clone()),
            JboMex::ConnectedMex(fore, con, m1, m2) => {
                JboMex::ConnectedMex(*fore, con.clone(), m1.clone(), m2.clone())
            }
            JboMex::QualifiedMex(q, m) => JboMex::QualifiedMex(q.clone(), m.clone()),
            JboMex::MexInt(n) => JboMex::MexInt(*n),
            JboMex::MexNumeralString(ns) => JboMex::MexNumeralString(ns.clone()),
            JboMex::MexLerfuString(ls) => JboMex::MexLerfuString(ls.clone()),
            JboMex::MexSelbri(vpred) => JboMex::MexSelbri(vpred.clone()),
            JboMex::MexSumti(t) => JboMex::MexSumti(Box::new((**t).clone())),
            JboMex::MexArray(ms) => JboMex::MexArray(ms.clone()),
        }
    }
}

impl std::fmt::Debug for JboMex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JboMex::Operation(op, ms) => f.debug_tuple("Operation").field(op).field(ms).finish(),
            JboMex::ConnectedMex(fore, con, m1, m2) => {
                f.debug_tuple("ConnectedMex").field(fore).field(con).field(m1).field(m2).finish()
            }
            JboMex::QualifiedMex(q, m) => f.debug_tuple("QualifiedMex").field(q).field(m).finish(),
            JboMex::MexInt(n) => f.debug_tuple("MexInt").field(n).finish(),
            JboMex::MexNumeralString(ns) => f.debug_tuple("MexNumeralString").field(ns).finish(),
            JboMex::MexLerfuString(ls) => f.debug_tuple("MexLerfuString").field(ls).finish(),
            JboMex::MexSelbri(_) => write!(f, "MexSelbri(<fn>)"),
            JboMex::MexSumti(t) => f.debug_tuple("MexSumti").field(t).finish(),
            JboMex::MexArray(ms) => f.debug_tuple("MexArray").field(ms).finish(),
        }
    }
}

impl PartialEq for JboMex {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (JboMex::Operation(op1, ms1), JboMex::Operation(op2, ms2)) => op1 == op2 && ms1 == ms2,
            (JboMex::ConnectedMex(f1, c1, m1a, m1b), JboMex::ConnectedMex(f2, c2, m2a, m2b)) => {
                f1 == f2 && c1 == c2 && m1a == m2a && m1b == m2b
            }
            (JboMex::QualifiedMex(q1, m1), JboMex::QualifiedMex(q2, m2)) => q1 == q2 && m1 == m2,
            (JboMex::MexInt(n1), JboMex::MexInt(n2)) => n1 == n2,
            (JboMex::MexNumeralString(ns1), JboMex::MexNumeralString(ns2)) => ns1 == ns2,
            (JboMex::MexLerfuString(ls1), JboMex::MexLerfuString(ls2)) => ls1 == ls2,
            (JboMex::MexSelbri(_), JboMex::MexSelbri(_)) => false,
            (JboMex::MexSumti(t1), JboMex::MexSumti(t2)) => t1 == t2,
            (JboMex::MexArray(ms1), JboMex::MexArray(ms2)) => ms1 == ms2,
            _ => false,
        }
    }
}

impl Eq for JboMex {}


/// JboOperator: Mathematical operator in semantic representation
pub enum JboOperator {
    ConnectedOperator(bool, crate::jbo_syntax::Connective, Box<JboOperator>, Box<JboOperator>),
    OpPermuted(i32, Box<JboOperator>),
    OpScalarNegated(String, Box<JboOperator>),
    OpMex(Box<JboMex>),
    OpSelbri(Box<JboVPred>),
    OpVUhU(String),
}

impl Clone for JboOperator {
    fn clone(&self) -> Self {
        match self {
            JboOperator::ConnectedOperator(fore, con, o1, o2) => {
                JboOperator::ConnectedOperator(*fore, con.clone(), o1.clone(), o2.clone())
            }
            JboOperator::OpPermuted(s, o) => JboOperator::OpPermuted(*s, o.clone()),
            JboOperator::OpScalarNegated(n, o) => {
                JboOperator::OpScalarNegated(n.clone(), o.clone())
            }
            JboOperator::OpMex(m) => JboOperator::OpMex(m.clone()),
            JboOperator::OpSelbri(vpred) => JboOperator::OpSelbri(vpred.clone()),
            JboOperator::OpVUhU(v) => JboOperator::OpVUhU(v.clone()),
        }
    }
}

impl std::fmt::Debug for JboOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JboOperator::ConnectedOperator(fore, con, o1, o2) => {
                f.debug_tuple("ConnectedOperator").field(fore).field(con).field(o1).field(o2).finish()
            }
            JboOperator::OpPermuted(s, o) => f.debug_tuple("OpPermuted").field(s).field(o).finish(),
            JboOperator::OpScalarNegated(n, o) => {
                f.debug_tuple("OpScalarNegated").field(n).field(o).finish()
            }
            JboOperator::OpMex(m) => f.debug_tuple("OpMex").field(m).finish(),
            JboOperator::OpSelbri(_) => write!(f, "OpSelbri(<fn>)"),
            JboOperator::OpVUhU(v) => f.debug_tuple("OpVUhU").field(v).finish(),
        }
    }
}

impl PartialEq for JboOperator {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (JboOperator::ConnectedOperator(f1, c1, o1a, o1b), JboOperator::ConnectedOperator(f2, c2, o2a, o2b)) => {
                f1 == f2 && c1 == c2 && o1a == o2a && o1b == o2b
            }
            (JboOperator::OpPermuted(s1, o1), JboOperator::OpPermuted(s2, o2)) => s1 == s2 && o1 == o2,
            (JboOperator::OpScalarNegated(n1, o1), JboOperator::OpScalarNegated(n2, o2)) => n1 == n2 && o1 == o2,
            (JboOperator::OpMex(m1), JboOperator::OpMex(m2)) => m1 == m2,
            (JboOperator::OpSelbri(_), JboOperator::OpSelbri(_)) => false,
            (JboOperator::OpVUhU(v1), JboOperator::OpVUhU(v2)) => v1 == v2,
            _ => false,
        }
    }
}

impl Eq for JboOperator {}


// ============================================================================
// Additional JboProp.hs functions
// ============================================================================

// Ported from: JboProp.hs :: andPred
/// Combine multiple predicates with conjunction
pub fn and_pred(preds: Vec<JboPred>) -> JboPred {
    Arc::new(move |x: &JboTerm| {
        let props: Vec<JboProp> = preds.iter().map(|pred| pred(x)).collect();
        crate::logic::big_and(props)
    })
}

// Ported from: JboProp.hs :: andMPred
/// Combine multiple predicates with conjunction, returning None if empty
pub fn and_m_pred(preds: Vec<JboPred>) -> Option<JboPred> {
    if preds.is_empty() {
        None
    } else {
        Some(and_pred(preds))
    }
}

// Ported from: JboProp.hs :: freeVars.
// Haskell uses `gtraverse_` over `JboProp`; Rust enumerates the same reachable semantic nodes.
pub fn free_vars(prop: &JboProp) -> Vec<JboTerm> {
    let mut vars = Vec::new();
    collect_free_vars(prop, &mut vars);
    vars
}

// Ported from: JboProp.hs :: freeVars (helper)
fn push_free_var(term: &JboTerm, vars: &mut Vec<JboTerm>) {
    if !vars.contains(term) {
        vars.push(term.clone());
    }
}

// Ported from: JboProp.hs :: freeVars (recursive collector)
fn collect_free_vars(prop: &JboProp, vars: &mut Vec<JboTerm>) {
    match prop {
        Prop::Rel(rel, terms) => {
            collect_free_vars_in_rel(rel, vars);
            for term in terms {
                collect_free_vars_in_term(term, vars);
            }
        }
        Prop::Not(p) | Prop::Modal(_, p) => collect_free_vars(p, vars),
        Prop::Connected(_, p1, p2) | Prop::NonLogConnected(_, p1, p2) => {
            collect_free_vars(p1, vars);
            collect_free_vars(p2, vars);
        }
        Prop::Quantified(_, restriction, body) => {
            if let Some(restriction) = restriction {
                collect_free_vars(&restriction(-1), vars);
            }
            collect_free_vars(&body(-1), vars);
        }
        Prop::Eet => {}
    }
}

// Ported from: JboProp.hs :: freeVars (relation traversal)
fn collect_free_vars_in_rel(rel: &JboRel, vars: &mut Vec<JboTerm>) {
    match rel {
        JboRel::Among(t) | JboRel::Moi(t, _) => collect_free_vars_in_term(t, vars),
        JboRel::Tanru(r1, r2) | JboRel::TanruConnective(_, r1, r2) => {
            collect_free_vars_in_rel(r1, vars);
            collect_free_vars_in_rel(r2, vars);
        }
        JboRel::AppliedRel(r, terms) => {
            collect_free_vars_in_rel(r, vars);
            for term in terms {
                collect_free_vars_in_term(term, vars);
            }
        }
        JboRel::PermutedRel(_, r) | JboRel::ScalarNegatedRel(_, r) | JboRel::ModalRel(_, r) => {
            collect_free_vars_in_rel(r, vars);
        }
        JboRel::OperatorRel(op) => collect_free_vars_in_operator(op, vars),
        JboRel::TagRel(tag) => collect_free_vars_in_tag(tag, vars),
        JboRel::AbsPred(_, npred) => collect_free_vars(&(npred.pred)(&vec![JboTerm::Unfilled; npred.arity]), vars),
        JboRel::AbsProp(_, p) => collect_free_vars(p, vars),
        JboRel::VPredRel(vpred) => collect_free_vars(&vpred(&[JboTerm::Unfilled]), vars),
        _ => {}
    }
}

// Ported from: JboProp.hs :: freeVars (term traversal)
fn collect_free_vars_in_term(term: &JboTerm, vars: &mut Vec<JboTerm>) {
    match term {
        JboTerm::Var(_) => push_free_var(term, vars),
        JboTerm::UnboundSumbasti(crate::jbo_syntax::SumtiAtom::MainBridiSumbasti(_)) => push_free_var(term, vars),
        JboTerm::Constant(_, args) => {
            for arg in args {
                collect_free_vars_in_term(arg, vars);
            }
        }
        JboTerm::PredNamed(pred) => collect_free_vars(&pred(&JboTerm::Unfilled), vars),
        JboTerm::Value(m) => collect_free_vars_in_mex(m, vars),
        JboTerm::QualifiedTerm(_, t) | JboTerm::TermWithSides(t, _) => collect_free_vars_in_term(t, vars),
        JboTerm::JoikedTerms(_, t1, t2) => {
            collect_free_vars_in_term(t1, vars);
            collect_free_vars_in_term(t2, vars);
        }
        _ => {}
    }
}

// Ported from: JboProp.hs :: subTerm (via gsubstituteIn)
/// Substitute a term for another term in a proposition
pub fn sub_term(old_term: &JboTerm, new_term: &JboTerm, prop: JboProp) -> JboProp {
    match prop {
        Prop::Rel(rel, terms) => {
            let new_terms = terms.iter().map(|t| sub_term_in_term(old_term, new_term, t)).collect();
            Prop::Rel(sub_term_in_rel(old_term, new_term, rel), new_terms)
        }
        Prop::Not(p) => Prop::Not(Box::new(sub_term(old_term, new_term, *p))),
        Prop::Connected(c, p1, p2) => Prop::Connected(
            c,
            Box::new(sub_term(old_term, new_term, *p1)),
            Box::new(sub_term(old_term, new_term, *p2)),
        ),
        Prop::NonLogConnected(c, p1, p2) => Prop::NonLogConnected(
            c,
            Box::new(sub_term(old_term, new_term, *p1)),
            Box::new(sub_term(old_term, new_term, *p2)),
        ),
        Prop::Modal(m, p) => Prop::Modal(
            sub_term_in_modal_op(old_term, new_term, m),
            Box::new(sub_term(old_term, new_term, *p)),
        ),
        Prop::Quantified(q, r, f) => {
            let old_term_clone = old_term.clone();
            let new_term_clone = new_term.clone();
            let old_term_for_body = old_term.clone();
            let new_term_for_body = new_term.clone();
            let new_r = r.map(move |pred| {
                let old_term = old_term_clone.clone();
                let new_term = new_term_clone.clone();
                std::sync::Arc::new(move |v| sub_term(&old_term, &new_term, pred(v))) as std::sync::Arc<dyn Fn(i32) -> JboProp + Send + Sync>
            });
            Prop::Quantified(q, new_r, std::sync::Arc::new(move |v| sub_term(&old_term_for_body, &new_term_for_body, f(v))))
        }
        Prop::Eet => Prop::Eet,
    }
}

// Ported from: JboProp.hs :: subTerm (term substitution helper)
fn sub_term_in_term(old_term: &JboTerm, new_term: &JboTerm, term: &JboTerm) -> JboTerm {
    if term == old_term {
        new_term.clone()
    } else {
        match term {
            JboTerm::Constant(n, args) => {
                let new_args = args.iter().map(|t| sub_term_in_term(old_term, new_term, t)).collect();
                JboTerm::Constant(*n, new_args)
            }
            JboTerm::Value(m) => JboTerm::Value(sub_term_in_mex(old_term, new_term, m.clone())),
            JboTerm::QualifiedTerm(qual, t) => {
                JboTerm::QualifiedTerm(qual.clone(), Box::new(sub_term_in_term(old_term, new_term, t)))
            }
            JboTerm::JoikedTerms(joik, t1, t2) => JboTerm::JoikedTerms(
                joik.clone(),
                Box::new(sub_term_in_term(old_term, new_term, t1)),
                Box::new(sub_term_in_term(old_term, new_term, t2)),
            ),
            JboTerm::TermWithSides(t, sides) => JboTerm::TermWithSides(
                Box::new(sub_term_in_term(old_term, new_term, t)),
                sides.clone(),
            ),
            _ => term.clone(),
        }
    }
}

// Ported from: JboProp.hs :: subTerm (mex substitution helper)
fn sub_term_in_mex(old_term: &JboTerm, new_term: &JboTerm, mex: JboMex) -> JboMex {
    match mex {
        JboMex::Operation(op, args) => JboMex::Operation(
            Box::new(sub_term_in_operator(old_term, new_term, *op)),
            args.into_iter().map(|m| sub_term_in_mex(old_term, new_term, m)).collect(),
        ),
        JboMex::ConnectedMex(fore, con, m1, m2) => JboMex::ConnectedMex(
            fore,
            con,
            Box::new(sub_term_in_mex(old_term, new_term, *m1)),
            Box::new(sub_term_in_mex(old_term, new_term, *m2)),
        ),
        JboMex::QualifiedMex(q, m) => JboMex::QualifiedMex(q, Box::new(sub_term_in_mex(old_term, new_term, *m))),
        JboMex::MexSelbri(vpred) => {
            let old_term = old_term.clone();
            let new_term = new_term.clone();
            JboMex::MexSelbri(Box::new(std::sync::Arc::new(move |terms| sub_term(&old_term, &new_term, vpred(terms)))))
        }
        JboMex::MexSumti(t) => JboMex::MexSumti(Box::new(sub_term_in_term(old_term, new_term, &t))),
        JboMex::MexArray(ms) => JboMex::MexArray(ms.into_iter().map(|m| sub_term_in_mex(old_term, new_term, m)).collect()),
        _ => mex,
    }
}

// Ported from: JboProp.hs :: subTerm (operator substitution helper)
fn sub_term_in_operator(old_term: &JboTerm, new_term: &JboTerm, op: JboOperator) -> JboOperator {
    match op {
        JboOperator::ConnectedOperator(fore, con, o1, o2) => JboOperator::ConnectedOperator(
            fore,
            con,
            Box::new(sub_term_in_operator(old_term, new_term, *o1)),
            Box::new(sub_term_in_operator(old_term, new_term, *o2)),
        ),
        JboOperator::OpPermuted(n, op) => JboOperator::OpPermuted(n, Box::new(sub_term_in_operator(old_term, new_term, *op))),
        JboOperator::OpScalarNegated(n, op) => JboOperator::OpScalarNegated(n, Box::new(sub_term_in_operator(old_term, new_term, *op))),
        JboOperator::OpMex(m) => JboOperator::OpMex(Box::new(sub_term_in_mex(old_term, new_term, *m))),
        JboOperator::OpSelbri(vpred) => {
            let old_term = old_term.clone();
            let new_term = new_term.clone();
            JboOperator::OpSelbri(Box::new(std::sync::Arc::new(move |terms| sub_term(&old_term, &new_term, vpred(terms)))))
        }
        JboOperator::OpVUhU(v) => JboOperator::OpVUhU(v),
    }
}

// Ported from: JboProp.hs :: freeVars (mex traversal)
fn collect_free_vars_in_mex(mex: &JboMex, vars: &mut Vec<JboTerm>) {
    match mex {
        JboMex::Operation(op, args) => {
            collect_free_vars_in_operator(op, vars);
            for arg in args {
                collect_free_vars_in_mex(arg, vars);
            }
        }
        JboMex::ConnectedMex(_, _, m1, m2) => {
            collect_free_vars_in_mex(m1, vars);
            collect_free_vars_in_mex(m2, vars);
        }
        JboMex::QualifiedMex(_, m) => collect_free_vars_in_mex(m, vars),
        JboMex::MexSelbri(vpred) => collect_free_vars(&vpred(&[JboTerm::Unfilled]), vars),
        JboMex::MexSumti(t) => collect_free_vars_in_term(t, vars),
        JboMex::MexArray(ms) => {
            for m in ms {
                collect_free_vars_in_mex(m, vars);
            }
        }
        _ => {}
    }
}

// Ported from: JboProp.hs :: freeVars (operator traversal)
fn collect_free_vars_in_operator(op: &JboOperator, vars: &mut Vec<JboTerm>) {
    match op {
        JboOperator::ConnectedOperator(_, _, o1, o2) => {
            collect_free_vars_in_operator(o1, vars);
            collect_free_vars_in_operator(o2, vars);
        }
        JboOperator::OpPermuted(_, op) | JboOperator::OpScalarNegated(_, op) => {
            collect_free_vars_in_operator(op, vars);
        }
        JboOperator::OpMex(m) => collect_free_vars_in_mex(m, vars),
        JboOperator::OpSelbri(vpred) => collect_free_vars(&vpred(&[JboTerm::Unfilled]), vars),
        JboOperator::OpVUhU(_) => {}
    }
}

// Ported from: JboProp.hs :: freeVars (tag traversal)
fn collect_free_vars_in_tag(tag: &JboTag, vars: &mut Vec<JboTerm>) {
    match tag {
        JboTag::DecoratedTagUnits(dtus) => {
            for dtu in dtus {
                collect_free_vars_in_tag_unit(&dtu.tag_unit, vars);
            }
        }
        JboTag::ConnectedTag(_, tag1, tag2) => {
            collect_free_vars_in_tag(tag1, vars);
            collect_free_vars_in_tag(tag2, vars);
        }
    }
}

// Ported from: JboProp.hs :: freeVars (tag unit traversal)
fn collect_free_vars_in_tag_unit(tag_unit: &JboTagUnit, vars: &mut Vec<JboTerm>) {
    match tag_unit {
        JboTagUnit::ROI(_, _, m) => collect_free_vars_in_mex(m, vars),
        JboTagUnit::FIhO(vpred) => collect_free_vars(&vpred(&[JboTerm::Unfilled]), vars),
        _ => {}
    }
}

// Ported from: JboProp.hs :: subTerm (modal operator substitution helper)
fn sub_term_in_modal_op(old_term: &JboTerm, new_term: &JboTerm, op: JboModalOp) -> JboModalOp {
    match op {
        JboModalOp::Tagged(tag, term) => JboModalOp::Tagged(
            tag,
            term.map(|t| sub_term_in_term(old_term, new_term, &t)),
        ),
        JboModalOp::WithEventAs(t) => JboModalOp::WithEventAs(sub_term_in_term(old_term, new_term, &t)),
        other => other,
    }
}

// Ported from: JboProp.hs :: subTerm (relation substitution helper)
fn sub_term_in_rel(old_term: &JboTerm, new_term: &JboTerm, rel: JboRel) -> JboRel {
    match rel {
        JboRel::Tanru(r1, r2) => JboRel::Tanru(
            Box::new(sub_term_in_rel(old_term, new_term, *r1)),
            Box::new(sub_term_in_rel(old_term, new_term, *r2)),
        ),
        JboRel::AppliedRel(r, terms) => JboRel::AppliedRel(
            Box::new(sub_term_in_rel(old_term, new_term, *r)),
            terms.iter().map(|t| sub_term_in_term(old_term, new_term, t)).collect(),
        ),
        JboRel::Among(t) => JboRel::Among(Box::new(sub_term_in_term(old_term, new_term, &t))),
        JboRel::Moi(t, s) => JboRel::Moi(Box::new(sub_term_in_term(old_term, new_term, &t)), s),
        JboRel::ScalarNegatedRel(s, r) => {
            JboRel::ScalarNegatedRel(s, Box::new(sub_term_in_rel(old_term, new_term, *r)))
        }
        JboRel::PermutedRel(n, r) => JboRel::PermutedRel(n, Box::new(sub_term_in_rel(old_term, new_term, *r))),
        JboRel::AbsPred(abs, npred) => {
            let old_term = old_term.clone();
            let new_term = new_term.clone();
            JboRel::AbsPred(abs, JboNPred {
                arity: npred.arity,
                pred: std::sync::Arc::new(move |terms| sub_term(&old_term, &new_term, (npred.pred)(terms))),
            })
        }
        JboRel::AbsProp(abs, prop) => JboRel::AbsProp(abs, Box::new(sub_term(old_term, new_term, *prop))),
        JboRel::TagRel(tag) => JboRel::TagRel(tag),
        JboRel::ModalRel(m, r) => JboRel::ModalRel(
            sub_term_in_modal_op(old_term, new_term, m),
            Box::new(sub_term_in_rel(old_term, new_term, *r)),
        ),
        _ => rel,
    }
}

// Ported from: JboProp.hs :: subRel (via gsubstituteIn)
/// Substitute a relation for another relation in a proposition
pub fn sub_rel(old_rel: &JboRel, new_rel: &JboRel, prop: JboProp) -> JboProp {
    match prop {
        Prop::Rel(rel, terms) => {
            let new_rel_val = if &rel == old_rel {
                new_rel.clone()
            } else {
                sub_rel_in_rel(old_rel, new_rel, rel)
            };
            let terms = terms.into_iter().map(|term| sub_rel_in_term(old_rel, new_rel, term)).collect();
            Prop::Rel(new_rel_val, terms)
        }
        Prop::Not(p) => Prop::Not(Box::new(sub_rel(old_rel, new_rel, *p))),
        Prop::Connected(c, p1, p2) => Prop::Connected(
            c,
            Box::new(sub_rel(old_rel, new_rel, *p1)),
            Box::new(sub_rel(old_rel, new_rel, *p2)),
        ),
        Prop::NonLogConnected(c, p1, p2) => Prop::NonLogConnected(
            c,
            Box::new(sub_rel(old_rel, new_rel, *p1)),
            Box::new(sub_rel(old_rel, new_rel, *p2)),
        ),
        Prop::Modal(m, p) => Prop::Modal(m, Box::new(sub_rel(old_rel, new_rel, *p))),
        Prop::Quantified(q, r, f) => {
            let old_rel_for_restriction = old_rel.clone();
            let new_rel_for_restriction = new_rel.clone();
            let old_rel_for_body = old_rel.clone();
            let new_rel_for_body = new_rel.clone();
            let new_r = r.map(move |pred| {
                let old_rel = old_rel_for_restriction.clone();
                let new_rel = new_rel_for_restriction.clone();
                std::sync::Arc::new(move |v| sub_rel(&old_rel, &new_rel, pred(v))) as std::sync::Arc<dyn Fn(i32) -> JboProp + Send + Sync>
            });
            Prop::Quantified(q, new_r, std::sync::Arc::new(move |v| sub_rel(&old_rel_for_body, &new_rel_for_body, f(v))))
        }
        Prop::Eet => Prop::Eet,
    }
}

// Ported from: JboProp.hs :: subRel (relation-in-relation substitution helper)
fn sub_rel_in_rel(old_rel: &JboRel, new_rel: &JboRel, rel: JboRel) -> JboRel {
    if &rel == old_rel {
        new_rel.clone()
    } else {
        match rel {
            JboRel::Among(t) => JboRel::Among(Box::new(sub_rel_in_term(old_rel, new_rel, *t))),
            JboRel::Tanru(r1, r2) => JboRel::Tanru(
                Box::new(sub_rel_in_rel(old_rel, new_rel, *r1)),
                Box::new(sub_rel_in_rel(old_rel, new_rel, *r2)),
            ),
            JboRel::AppliedRel(r, terms) => JboRel::AppliedRel(
                Box::new(sub_rel_in_rel(old_rel, new_rel, *r)),
                terms.into_iter().map(|term| sub_rel_in_term(old_rel, new_rel, term)).collect(),
            ),
            JboRel::TanruConnective(con, r1, r2) => JboRel::TanruConnective(
                con,
                Box::new(sub_rel_in_rel(old_rel, new_rel, *r1)),
                Box::new(sub_rel_in_rel(old_rel, new_rel, *r2)),
            ),
            JboRel::PermutedRel(n, r) => JboRel::PermutedRel(n, Box::new(sub_rel_in_rel(old_rel, new_rel, *r))),
            JboRel::Moi(t, moi) => JboRel::Moi(Box::new(sub_rel_in_term(old_rel, new_rel, *t)), moi),
            JboRel::OperatorRel(op) => JboRel::OperatorRel(Box::new(sub_rel_in_operator(old_rel, new_rel, *op))),
            JboRel::ScalarNegatedRel(s, r) => {
                JboRel::ScalarNegatedRel(s, Box::new(sub_rel_in_rel(old_rel, new_rel, *r)))
            }
            JboRel::VPredRel(vpred) => {
                let old_rel = old_rel.clone();
                let new_rel = new_rel.clone();
                JboRel::VPredRel(std::sync::Arc::new(move |terms| {
                    sub_rel(&old_rel, &new_rel, vpred(terms))
                }))
            }
            JboRel::AbsPred(abs, npred) => {
                let old_rel = old_rel.clone();
                let new_rel = new_rel.clone();
                JboRel::AbsPred(abs, JboNPred {
                    arity: npred.arity,
                    pred: std::sync::Arc::new(move |terms| sub_rel(&old_rel, &new_rel, (npred.pred)(terms))),
                })
            }
            JboRel::AbsProp(abs, prop) => JboRel::AbsProp(abs, Box::new(sub_rel(old_rel, new_rel, *prop))),
            JboRel::ModalRel(m, r) => JboRel::ModalRel(
                sub_rel_in_modal_op(old_rel, new_rel, m),
                Box::new(sub_rel_in_rel(old_rel, new_rel, *r)),
            ),
            _ => rel,
        }
    }
}

// Ported from: JboProp.hs :: subRel (relation-in-term substitution helper)
fn sub_rel_in_term(old_rel: &JboRel, new_rel: &JboRel, term: JboTerm) -> JboTerm {
    match term {
        JboTerm::Constant(n, args) => JboTerm::Constant(
            n,
            args.into_iter().map(|term| sub_rel_in_term(old_rel, new_rel, term)).collect(),
        ),
        JboTerm::PredNamed(pred) => {
            let old_rel = old_rel.clone();
            let new_rel = new_rel.clone();
            JboTerm::PredNamed(std::sync::Arc::new(move |term| sub_rel(&old_rel, &new_rel, pred(term))))
        }
        JboTerm::Value(m) => JboTerm::Value(sub_rel_in_mex(old_rel, new_rel, m)),
        JboTerm::QualifiedTerm(q, t) => JboTerm::QualifiedTerm(q, Box::new(sub_rel_in_term(old_rel, new_rel, *t))),
        JboTerm::JoikedTerms(joik, t1, t2) => JboTerm::JoikedTerms(
            joik,
            Box::new(sub_rel_in_term(old_rel, new_rel, *t1)),
            Box::new(sub_rel_in_term(old_rel, new_rel, *t2)),
        ),
        JboTerm::TermWithSides(t, sides) => JboTerm::TermWithSides(
            Box::new(sub_rel_in_term(old_rel, new_rel, *t)),
            sides,
        ),
        _ => term,
    }
}

// Ported from: JboProp.hs :: subRel (relation-in-mex substitution helper)
fn sub_rel_in_mex(old_rel: &JboRel, new_rel: &JboRel, mex: JboMex) -> JboMex {
    match mex {
        JboMex::Operation(op, args) => JboMex::Operation(
            Box::new(sub_rel_in_operator(old_rel, new_rel, *op)),
            args.into_iter().map(|m| sub_rel_in_mex(old_rel, new_rel, m)).collect(),
        ),
        JboMex::ConnectedMex(fore, con, m1, m2) => JboMex::ConnectedMex(
            fore,
            con,
            Box::new(sub_rel_in_mex(old_rel, new_rel, *m1)),
            Box::new(sub_rel_in_mex(old_rel, new_rel, *m2)),
        ),
        JboMex::QualifiedMex(q, m) => JboMex::QualifiedMex(q, Box::new(sub_rel_in_mex(old_rel, new_rel, *m))),
        JboMex::MexSelbri(vpred) => {
            let old_rel = old_rel.clone();
            let new_rel = new_rel.clone();
            JboMex::MexSelbri(Box::new(std::sync::Arc::new(move |terms| sub_rel(&old_rel, &new_rel, vpred(terms)))))
        }
        JboMex::MexSumti(t) => JboMex::MexSumti(Box::new(sub_rel_in_term(old_rel, new_rel, *t))),
        JboMex::MexArray(ms) => JboMex::MexArray(ms.into_iter().map(|m| sub_rel_in_mex(old_rel, new_rel, m)).collect()),
        _ => mex,
    }
}

// Ported from: JboProp.hs :: subRel (relation-in-operator substitution helper)
fn sub_rel_in_operator(old_rel: &JboRel, new_rel: &JboRel, op: JboOperator) -> JboOperator {
    match op {
        JboOperator::ConnectedOperator(fore, con, o1, o2) => JboOperator::ConnectedOperator(
            fore,
            con,
            Box::new(sub_rel_in_operator(old_rel, new_rel, *o1)),
            Box::new(sub_rel_in_operator(old_rel, new_rel, *o2)),
        ),
        JboOperator::OpPermuted(n, op) => JboOperator::OpPermuted(n, Box::new(sub_rel_in_operator(old_rel, new_rel, *op))),
        JboOperator::OpScalarNegated(n, op) => JboOperator::OpScalarNegated(n, Box::new(sub_rel_in_operator(old_rel, new_rel, *op))),
        JboOperator::OpMex(m) => JboOperator::OpMex(Box::new(sub_rel_in_mex(old_rel, new_rel, *m))),
        JboOperator::OpSelbri(vpred) => {
            let old_rel = old_rel.clone();
            let new_rel = new_rel.clone();
            JboOperator::OpSelbri(Box::new(std::sync::Arc::new(move |terms| sub_rel(&old_rel, &new_rel, vpred(terms)))))
        }
        _ => op,
    }
}

// Ported from: JboProp.hs :: subRel (relation-in-modal substitution helper)
fn sub_rel_in_modal_op(old_rel: &JboRel, new_rel: &JboRel, modal: JboModalOp) -> JboModalOp {
    match modal {
        JboModalOp::Tagged(tag, term) => JboModalOp::Tagged(
            tag,
            term.map(|term| sub_rel_in_term(old_rel, new_rel, term)),
        ),
        JboModalOp::WithEventAs(term) => JboModalOp::WithEventAs(sub_rel_in_term(old_rel, new_rel, term)),
        _ => modal,
    }
}

// Ported from: JboProp.hs :: stripTermSides
/// Strip term-attached sides (SEI) for semantic operations; preserves inner term
pub fn strip_term_sides(term: &JboTerm) -> JboTerm {
    match term {
        JboTerm::TermWithSides(t, _) => strip_term_sides(t),
        JboTerm::Constant(n, args) => {
            let stripped_args: Vec<_> = args.iter().map(strip_term_sides).collect();
            JboTerm::Constant(*n, stripped_args)
        }
        _ => term.clone(),
    }
}

// Ported from: JboProp.hs :: reWrapTermSides
/// Re-attach sides from original term to (possibly modified) inner term
pub fn re_wrap_term_sides(original: &JboTerm, inner: JboTerm) -> JboTerm {
    match original {
        JboTerm::TermWithSides(t, sides) => JboTerm::TermWithSides(
            Box::new(re_wrap_term_sides(t, inner)),
            sides.clone(),
        ),
        _ => inner,
    }
}

// Ported from: JboProp.hs :: isAmong
/// Create an "among" predicate
pub fn is_among(term: JboTerm) -> JboPred {
    Arc::new(move |x: &JboTerm| {
        Prop::Rel(JboRel::Among(Box::new(term.clone())), vec![x.clone()])
    })
}

// Ported from: JboParse.hs :: nonveridicialPred
/// Wrap a predicate in a non-veridical modal
pub fn nonveridicial_pred(p: JboPred) -> JboPred {
    Arc::new(move |t: &JboTerm| {
        Prop::Modal(JboModalOp::NonVeridical, Box::new(p(t)))
    })
}

// Ported from: JboProp.hs :: connToFOL
/// Convert logical connective to FOL proposition
pub fn conn_to_fol(conn: &crate::jbo_syntax::LogJboConnective, p1: JboProp, p2: JboProp) -> JboProp {
    use crate::logic::Connective;

    let crate::jbo_syntax::LogJboConnective { b1, c, b2 } = conn;

    match (b1, *c, b2) {
        (true, 'e', true) => Prop::Connected(Connective::And, Box::new(p1), Box::new(p2)),
        (true, 'a', true) => Prop::Connected(Connective::Or, Box::new(p1), Box::new(p2)),
        (false, 'a', true) => Prop::Connected(Connective::Impl, Box::new(p1), Box::new(p2)),
        (true, 'a', false) => Prop::Connected(Connective::Impl, Box::new(p2), Box::new(p1)),
        (true, 'o', true) => Prop::Connected(Connective::Equiv, Box::new(p1), Box::new(p2)),
        (true, 'u', true) => p1,
        (true, 'U', true) => p2,
        (false, c, b2) => {
            let neg_conn = crate::jbo_syntax::LogJboConnective { b1: true, c, b2: *b2 };
            conn_to_fol(&neg_conn, Prop::Not(Box::new(p1)), p2)
        }
        (b1, c, false) => {
            let neg_conn = crate::jbo_syntax::LogJboConnective { b1: *b1, c, b2: true };
            conn_to_fol(&neg_conn, p1, Prop::Not(Box::new(p2)))
        }
        _ => Prop::Eet,
    }
}

// Ported from: JboProp.hs :: joikToFOL
/// Convert joik to FOL proposition
pub fn joik_to_fol(joik: String, p1: JboProp, p2: JboProp) -> JboProp {
    Prop::NonLogConnected(joik, Box::new(p1), Box::new(p2))
}


