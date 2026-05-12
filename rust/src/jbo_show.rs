//! Output formatting from [JboShow.hs](../JboShow.hs).
//!
//! This module ports the `JboShow` and `LogShow` instances used by the Haskell CLI to render
//! semantic `JboProp.hs` values as canonical Lojban and logical debug text. Rust passes explicit
//! formatter context (`ShowBinding`, lambda-variable slices) where Haskell uses `Bindful.hs` state
//! helpers such as `withShuntedRelVar`.

use crate::jbo_prop::{JboFragment, JboMex, JboModalOp, JboOperator, JboQuantifier, JboRel, JboTerm, SideType, Texticule};
use crate::jbo_syntax::{Lerfu, SumtiAtom};
use crate::logic::Prop;

pub type JboProp = Prop<JboRel, JboTerm, String, JboModalOp, JboQuantifier>;

#[derive(Clone, Copy, PartialEq, Eq)]
enum ShowBinding {
    SVar(i32),
    SAss(i32),
    SRel(i32),
    SRVar(i32),
    SRAss(i32),
}

// Ported from: JboShow.hs :: nonJboQuoteDelimiter
fn non_jbo_quote_delimiter(s: &str) -> String {
    let mut n = 0;
    loop {
        let delimiter = if n == 0 {
            "zoi".to_string()
        } else {
            format!("zoik{}", "yk".repeat(n - 1))
        };
        if !s.contains(&delimiter) {
            return delimiter;
        }
        n += 1;
    }
}

// Ported from: JboShow.hs :: jbonum
/// Convert integer to Lojban digit string
pub fn jbonum(n: i32) -> String {
    match n {
        0 => "no".to_string(),
        1 => "pa".to_string(),
        2 => "re".to_string(),
        3 => "ci".to_string(),
        4 => "vo".to_string(),
        5 => "mu".to_string(),
        6 => "xa".to_string(),
        7 => "ze".to_string(),
        8 => "bi".to_string(),
        9 => "so".to_string(),
        _ if n >= 10 => format!("{}{}", jbonum(n / 10), jbonum(n % 10)),
        _ => format!("{}", n),
    }
}

// Ported from: JboShow.hs :: vowelnum
/// Convert number to vowel string (a, e, i, o, u)
pub fn vowelnum(n: i32) -> &'static str {
    match n {
        1 => "a",
        2 => "e",
        3 => "i",
        4 => "o",
        5 => "u",
        _ => "a",
    }
}

// Ported from: JboShow.hs :: seToStr
/// Convert SE index to Lojban SE cmavo
pub fn se_to_str(n: i32) -> String {
    match n {
        2 => "se".to_string(),
        3 => "te".to_string(),
        4 => "ve".to_string(),
        5 => "xe".to_string(),
        _ => format!("se xi {}", jbonum(n)),
    }
}

// Ported from: JboShow.hs :: faToStr
/// Convert FA index to Lojban FA cmavo
pub fn fa_to_str(n: i32) -> String {
    match n {
        1 => "fa".to_string(),
        2 => "fe".to_string(),
        3 => "fi".to_string(),
        4 => "fo".to_string(),
        5 => "fu".to_string(),
        _ => format!("fa xi {}", jbonum(n)),
    }
}

// Ported from: JboShow.hs :: bracket
/// Bracket a string with matching delimiters
pub fn bracket(c: char, s: &str) -> String {
    let end = match c {
        '(' => ")",
        '[' => "]",
        '{' => "}",
        '<' => ">",
        '«' => "»",
        '"' => "\"",
        '\'' => "'",
        ' ' => " ",
        _ => "",
    };
    format!("{}{}{}", c, s, end)
}

// Ported from: JboShow.hs :: jbobracket
/// Bracket with Lojban terminators
pub fn jbobracket(l: &str, r: &str, s: &str) -> String {
    format!("{} {} {}", l, s, r)
}

// Ported from: JboShow.hs :: instance JboShow JboOperator (jboshow method)
fn jboshow_operator(op: &JboOperator) -> String {
    jboshow_operator_with_context(op, None, &[])
}

// Ported from: JboShow.hs :: instance JboShow JboOperator (jboshow method)
// Rust adaptation: explicit context parameters instead of Bindful monad state
fn jboshow_operator_with_context(op: &JboOperator, rel_var: Option<ShowBinding>, bindings: &[(i32, ShowBinding)]) -> String {
    match op {
        JboOperator::ConnectedOperator(_, con, o1, o2) => format!(
            "ke {} ke'e {} ke {} ke'e",
            jboshow_operator_with_context(o1, rel_var, bindings),
            jboshow_connective(".", con),
            jboshow_operator_with_context(o2, rel_var, bindings)
        ),
        JboOperator::OpPermuted(s, op) => format!("{} {}", se_to_str(*s), jboshow_operator_with_context(op, rel_var, bindings)),
        JboOperator::OpScalarNegated(n, op) => format!("{} {}", n, jboshow_operator_with_context(op, rel_var, bindings)),
        JboOperator::OpMex(m) => jbobracket("ma'o", "te'u", &jboshow_mex_with_context(m, rel_var, bindings)),
        JboOperator::OpSelbri(s) => jbobracket(
            "na'u",
            "te'u",
            &jboshow_pred_prop_with_rel_context(&s(&[JboTerm::BoundVar(0)]), 0, rel_var),
        ),
        JboOperator::OpVUhU(s) => s.clone(),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboOperator (logshow method)
fn logshow_operator(op: &JboOperator) -> String {
    match op {
        JboOperator::ConnectedOperator(_, con, o1, o2) => format!(
            "<{}>{}<{}>",
            logshow_operator(o1),
            logshow_connective(".", con),
            logshow_operator(o2)
        ),
        JboOperator::OpPermuted(s, op) => format!("{} {}", se_to_str(*s), logshow_operator(op)),
        JboOperator::OpScalarNegated(n, op) => format!("{{{}}}({})", n, logshow_operator(op)),
        JboOperator::OpMex(m) => bracket('[', &logshow_mex(m)),
        JboOperator::OpSelbri(s) => bracket('[', &logshow_prop(&s(&[JboTerm::BoundVar(0)]))),
        JboOperator::OpVUhU(s) => format!("{{{}}}", s),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboMex (jboshow method), numeric cases only
fn jboshow_mex_number(mex: &JboMex) -> String {
    match mex {
        JboMex::MexNumeralString(ns) => ns
            .iter()
            .map(|n| match n {
                crate::jbo_syntax::Numeral::PA(s) => s.clone(),
                crate::jbo_syntax::Numeral::NumeralLerfu(l) => jboshow_lerfu(l),
            })
            .collect::<Vec<_>>()
            .join(" "),
        JboMex::MexLerfuString(ls) => jboshow_lerfu_string(ls),
        JboMex::MexInt(n) => jbonum(*n),
        _ => "<mex>".to_string(),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboMex (jboshow method)
fn jboshow_mex(mex: &JboMex) -> String {
    jboshow_mex_with_context(mex, None, &[])
}

// Ported from: JboShow.hs :: instance JboShow JboMex (jboshow method)
// Rust adaptation: explicit context parameters instead of Bindful monad state
fn jboshow_mex_with_context(mex: &JboMex, rel_var: Option<ShowBinding>, bindings: &[(i32, ShowBinding)]) -> String {
    match mex {
        JboMex::MexNumeralString(_) | JboMex::MexInt(_) => format!("{} boi", jboshow_mex_number(mex)),
        JboMex::MexLerfuString(ls) => format!("{} boi", jboshow_lerfu_string(ls)),
        JboMex::Operation(op, args) => {
            let args = args.iter().map(|m| jboshow_mex_with_context(m, rel_var, bindings)).collect::<Vec<_>>().join(" ");
            format!("pe'o {} {} ku'e", jboshow_operator_with_context(op, rel_var, bindings), args)
        }
        JboMex::ConnectedMex(_, con, m1, m2) => format!(
            "vei {} ve'o {} vei {} ve'o",
            jboshow_mex_with_context(m1, rel_var, bindings),
            jboshow_connective(".", con),
            jboshow_mex_with_context(m2, rel_var, bindings)
        ),
        JboMex::QualifiedMex(qual, m) => {
            let qualifier = match qual {
                crate::jbo_syntax::SumtiQualifier::LAhE(l) => l.clone(),
                crate::jbo_syntax::SumtiQualifier::NAhE_BO(n) => format!("{} bo", n),
            };
            format!("{} {} lu'u", qualifier, jboshow_mex_with_context(m, rel_var, bindings))
        }
        JboMex::MexSelbri(s) => jbobracket(
            "ni'e",
            "te'u",
            &jboshow_pred_prop_with_rel_context(&s(&[JboTerm::BoundVar(0)]), 0, rel_var),
        ),
        JboMex::MexArray(ms) => format!(
            "jo'i {} te'u",
            ms.iter().map(|m| jboshow_mex_with_context(m, rel_var, bindings)).collect::<Vec<_>>().join(" ")
        ),
        JboMex::MexSumti(t) => format!("mo'e {} te'u", jboshow_term_with_context(t, &[], rel_var, bindings)),
    }
}

// Ported from: JboShow.hs :: instance JboShow LerfuString (jboshow method)
fn jboshow_lerfu_string(ls: &[Lerfu]) -> String {
    ls.iter().map(jboshow_lerfu).collect::<Vec<_>>().join(" ")
}

// Ported from: JboShow.hs :: instance JboShow Lerfu (jboshow method)
fn jboshow_lerfu(l: &Lerfu) -> String {
    match l {
        Lerfu::LerfuChar(c) if matches!(c, 'a' | 'e' | 'i' | 'o' | 'u') => format!("{}bu", c),
        Lerfu::LerfuChar('y') => "y bu".to_string(),
        Lerfu::LerfuChar('h') => "y'y".to_string(),
        Lerfu::LerfuChar(c) if c.is_ascii_digit() => jbonum(c.to_digit(10).unwrap() as i32),
        Lerfu::LerfuChar(c) => format!("{}y", c),
        Lerfu::LerfuPA(p) => p.clone(),
        Lerfu::LerfuValsi(v) => format!("{} bu", v),
        Lerfu::LerfuShift(s) => s.clone(),
        Lerfu::LerfuShifted(lau, l) => format!("{} {}", lau, jboshow_lerfu(l)),
        Lerfu::LerfuComposite(ls) => jbobracket("tei", "foi", &jboshow_lerfu_string(ls)),
    }
}

// Ported from: JboShow.hs :: instance JboShow LerfuString (logshow method)
fn logshow_lerfu_string(ls: &[Lerfu]) -> String {
    ls.iter().map(logshow_lerfu).collect::<String>()
}

// Ported from: JboShow.hs :: instance JboShow Lerfu (logshow method)
fn logshow_lerfu(l: &Lerfu) -> String {
    match l {
        Lerfu::LerfuChar(c) => c.to_string(),
        Lerfu::LerfuPA(p) | Lerfu::LerfuValsi(p) | Lerfu::LerfuShift(p) => bracket('{', p),
        Lerfu::LerfuShifted(lau, l) => format!("{}{}", bracket('{', lau), bracket('(', &logshow_lerfu(l))),
        Lerfu::LerfuComposite(ls) => bracket('[', &ls.iter().map(logshow_lerfu).collect::<Vec<_>>().join(","))
    }
}

// Rust-only: Debug helper for Lerfu, not present in Haskell
fn logshow_lerfu_debug(l: &Lerfu) -> String {
    match l {
        Lerfu::LerfuChar(c) => format!("LerfuChar '{}'", c),
        Lerfu::LerfuPA(p) => format!("LerfuPA {:?}", p),
        Lerfu::LerfuValsi(v) => format!("LerfuValsi {:?}", v),
        Lerfu::LerfuShift(s) => format!("LerfuShift {:?}", s),
        Lerfu::LerfuShifted(lau, l) => format!("LerfuShifted {:?} ({})", lau, logshow_lerfu_debug(l)),
        Lerfu::LerfuComposite(ls) => format!("LerfuComposite [{}]", ls.iter().map(logshow_lerfu_debug).collect::<Vec<_>>().join(",")),
    }
}

// Ported from: JboShow.hs :: instance JboShow SumtiAtom (jboshow method)
// Covers SAss (ko'a/ko'e/..., fo'a/fo'e/...) and anaphora (ri, ra)
fn jboshow_sumti_atom(atom: &SumtiAtom) -> String {
    match atom {
        SumtiAtom::Assignable(n) if *n <= 5 => format!("ko'{}", vowelnum(*n)),
        SumtiAtom::Assignable(n) if *n <= 10 => format!("fo'{}", vowelnum(*n - 5)),
        SumtiAtom::Assignable(n) => format!("ko'a xi {}", jbonum(*n)),
        SumtiAtom::Ri(0 | 1) => "ri".to_string(),
        SumtiAtom::Ri(n) => format!("ri xi {}", jbonum(*n)),
        SumtiAtom::Ra(r) => r.clone(),
        SumtiAtom::MainBridiSumbasti(n) if *n <= 5 => format!("vo'{}", vowelnum(*n)),
        SumtiAtom::MainBridiSumbasti(n) => format!("vo'a xi {}", jbonum(*n)),
        SumtiAtom::LerfuString(ls) => jboshow_lerfu_string(ls),
        _ => format!("{:?}", atom),
    }
}

// Ported from: JboShow.hs :: instance JboShow SumtiAtom (logshow method)
fn logshow_sumti_atom(atom: &SumtiAtom) -> String {
    match atom {
        SumtiAtom::LerfuString(ls) => logshow_lerfu_string(ls),
        _ => bracket('{', &jboshow_sumti_atom(atom)),
    }
}

// Rust-only: Debug helper for syntax-level Operator, not present in Haskell
fn logshow_syntax_operator(op: &crate::jbo_syntax::Operator) -> String {
    match op {
        crate::jbo_syntax::Operator::OpVUhU(s) => format!("OpVUhU \"{}\"", s),
        _ => format!("{:?}", op),
    }
}

// Rust-only: Debug helper for syntax-level Mex, not present in Haskell
fn logshow_syntax_mex(mex: &crate::jbo_syntax::Mex) -> String {
    match mex {
        crate::jbo_syntax::Mex::Operation(op, args) => {
            let args = args.iter().map(logshow_syntax_mex).collect::<Vec<_>>().join(",");
            format!("Operation ({}) [{}]", logshow_syntax_operator(op), args)
        }
        crate::jbo_syntax::Mex::MexNumeralString(ns) => {
            let digits = ns
                .iter()
                .map(|n| match n {
                    crate::jbo_syntax::Numeral::PA(s) => s.clone(),
                    crate::jbo_syntax::Numeral::NumeralLerfu(l) => format!("{:?}", l),
                })
                .collect::<Vec<_>>()
                .join("");
            match digits.as_str() {
                "no" => "MexInt 0".to_string(),
                "pa" => "MexInt 1".to_string(),
                "re" => "MexInt 2".to_string(),
                "ci" => "MexInt 3".to_string(),
                "vo" => "MexInt 4".to_string(),
                "mu" => "MexInt 5".to_string(),
                "xa" => "MexInt 6".to_string(),
                "ze" => "MexInt 7".to_string(),
                "bi" => "MexInt 8".to_string(),
                "so" => "MexInt 9".to_string(),
                _ => format!("MexNumeralString {:?}", ns),
            }
        }
        crate::jbo_syntax::Mex::MexInt(n) => format!("MexInt {}", n),
        crate::jbo_syntax::Mex::MexLerfuString(ls) => format!("MexLerfuString [{}]", ls.iter().map(logshow_lerfu_debug).collect::<Vec<_>>().join(",")),
        _ => format!("{:?}", mex),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboMex (logshow method)
fn logshow_mex(mex: &JboMex) -> String {
    match mex {
        JboMex::MexNumeralString(ns) => bracket('(', &ns
            .iter()
            .map(|n| match n {
                crate::jbo_syntax::Numeral::PA(s) => s.clone(),
                crate::jbo_syntax::Numeral::NumeralLerfu(l) => jboshow_lerfu(l),
            })
            .collect::<Vec<_>>()
            .join(" ")),
        JboMex::MexInt(n) => n.to_string(),
        JboMex::MexLerfuString(ls) => bracket('(', &jboshow_lerfu_string(ls)),
        JboMex::Operation(op, args) => {
            let args = args.iter().map(logshow_mex).collect::<Vec<_>>().join(",");
            format!("{}({})", logshow_operator(op), args)
        }
        JboMex::ConnectedMex(_, con, m1, m2) => format!(
            "({}){}({})",
            logshow_mex(m1),
            logshow_connective(".", con),
            logshow_mex(m2)
        ),
        JboMex::QualifiedMex(qual, m) => {
            let qualifier = match qual {
                crate::jbo_syntax::SumtiQualifier::LAhE(l) => l.clone(),
                crate::jbo_syntax::SumtiQualifier::NAhE_BO(n) => format!("{} bo", n),
            };
            format!("{{{}}}({})", qualifier, logshow_mex(m))
        }
        JboMex::MexSelbri(s) => bracket('[', &logshow_prop(&s(&[JboTerm::BoundVar(0)]))),
        JboMex::MexArray(ms) => bracket(
            '[',
            &ms.iter().map(logshow_mex).collect::<Vec<_>>().join(","),
        ),
        _ => "<mex>".to_string(),
    }
}

// Rust-only: Syntax-level operator display helper, not present in Haskell
fn jboshow_syntax_operator(op: &crate::jbo_syntax::Operator) -> String {
    match op {
        crate::jbo_syntax::Operator::OpVUhU(s) => s.clone(),
        _ => "<operator>".to_string(),
    }
}

// Rust-only: Syntax-level Mex display helper, not present in Haskell
fn jboshow_syntax_mex(mex: &crate::jbo_syntax::Mex) -> String {
    match mex {
        crate::jbo_syntax::Mex::MexNumeralString(ns) => ns
            .iter()
            .map(|n| match n {
                crate::jbo_syntax::Numeral::PA(s) => s.clone(),
                crate::jbo_syntax::Numeral::NumeralLerfu(l) => format!("{:?}", l),
            })
            .collect::<Vec<_>>()
            .join(" "),
        crate::jbo_syntax::Mex::MexInt(n) => jbonum(*n),
        crate::jbo_syntax::Mex::Operation(op, args) => {
            let args = args.iter().map(jboshow_syntax_mex).collect::<Vec<_>>().join(" boi ");
            format!("pe'o {} {} boi ku'e", jboshow_syntax_operator(op), args)
        }
        _ => "<mex>".to_string(),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboQuantifier (jboshow method)
fn jboshow_quantifier(q: &JboQuantifier) -> String {
    match q {
        JboQuantifier::QuestionQuantifier => "ma".to_string(),
        JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists) => "su'o".to_string(),
        JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Forall) => "ro".to_string(),
        JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(n)) => jbonum(*n),
        JboQuantifier::MexQuantifier(m) => jboshow_mex(m),
        JboQuantifier::RelQuantifier(q) => jboshow_quantifier(q),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboQuantifier (logshow method)
fn logshow_quantifier(q: &JboQuantifier) -> String {
    match q {
        JboQuantifier::QuestionQuantifier => "?".to_string(),
        JboQuantifier::LojQuantifier(q) => q.to_string(),
        JboQuantifier::MexQuantifier(m) => logshow_mex(m),
        JboQuantifier::RelQuantifier(q) => format!("RelQuantifier({})", logshow_quantifier(q)),
    }
}

// Rust-only: Helper for formatting quantified variable prefixes in logical output
fn format_quantified_prefix(q: &JboQuantifier, name: &str) -> String {
    match q {
        JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists)
        | JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Forall) => {
            format!("{}{}", logshow_quantifier(q), name)
        }
        _ => format!("{} {}", logshow_quantifier(q), name),
    }
}

// Ported from: JboShow.hs :: instance JboShow ShowBindable (jboshow method), SLambda case
fn jboshow_lambda_var(n: i32) -> String {
    match n.abs() {
        1 => "ce'u".to_string(),
        n => format!("ce'u xi {}", jbonum(n)),
    }
}

// Ported from: JboShow.hs :: instance JboShow ShowBindable (jboshow method), SVar case
// Haskell: SVar n | n <= 3 -> "d" ++ vowelnum n
fn jboshow_bound_var(n: i32) -> String {
    match n {
        1 => "da".to_string(),
        2 => "de".to_string(),
        3 => "di".to_string(),
        _ => format!("da xi {}", jbonum(n)),
    }
}

// Ported from: JboShow.hs :: instance JboShow ShowBindable (jboshow method), SRAss case
// Haskell: SRAss n | n <= 5 -> "brod" ++ vowelnum n
fn jboshow_bound_rvar(n: i32) -> String {
    match n {
        1 => "broda".to_string(),
        2 => "brode".to_string(),
        3 => "brodi".to_string(),
        4 => "brodo".to_string(),
        5 => "brodu".to_string(),
        _ => format!("broda xi {}", jbonum(n)),
    }
}

// Ported from: JboShow.hs :: instance JboShow ShowBindable (jboshow method), SRVar case
// Haskell: SRVar n -> if n <= 3 then "bu'" ++ vowelnum n
fn jboshow_unassigned_rvar(n: i32) -> String {
    match n {
        1 => "bu'a".to_string(),
        2 => "bu'e".to_string(),
        3 => "bu'i".to_string(),
        _ => format!("bu'a xi {}", jbonum(n)),
    }
}


// Ported from: JboShow.hs :: instance JboShow Texticule (logshow method)
fn logshow_texticule(texticule: &Texticule) -> String {
    match texticule {
        Texticule::TexticuleFrag(fragment) => match fragment {
            JboFragment::JboFragTerms(terms) => format!("[Fragment: {}]", terms.iter().map(logshow_term).collect::<Vec<_>>().join(",")),
            JboFragment::JboFragUnparsed(_) => "[Fragment]".to_string(),
        },
        Texticule::TexticuleProp(prop) => logshow_prop(prop),
        Texticule::TexticuleSide(_, inner) => logshow_texticule(inner),
    }
}

// Ported from: JboShow.hs :: instance JboShow [Texticule] (logshow method)
fn logshow_text(text: &[Texticule]) -> String {
    text.iter().map(logshow_texticule).collect::<Vec<_>>().join("\n")
}

// Ported from: JboShow.hs :: instance JboShow Texticule (jboshow method)
pub fn jboshow_texticule(texticule: &Texticule) -> String {
    match texticule {
        Texticule::TexticuleFrag(fragment) => match fragment {
            JboFragment::JboFragTerms(terms) => terms.iter().map(jboshow_term).collect::<Vec<_>>().join(" "),
            JboFragment::JboFragUnparsed(_) => "li'o".to_string(),
        },
        Texticule::TexticuleProp(prop) => jboshow_prop(prop),
        Texticule::TexticuleSide(_, inner) => jboshow_texticule(inner),
    }
}

// Rust adaptation: Collects side texticules from TermWithSides for display
// Haskell stores sides differently; this mirrors the display behavior
fn collect_term_side_texticules_from_texticule(texticule: &Texticule, out: &mut Vec<Texticule>) {
    match texticule {
        Texticule::TexticuleFrag(JboFragment::JboFragTerms(terms)) => {
            for term in terms {
                collect_term_side_texticules_from_term(term, out);
            }
        }
        Texticule::TexticuleProp(prop) => collect_term_side_texticules(prop, out),
        Texticule::TexticuleSide(_, inner) => collect_term_side_texticules_from_texticule(inner, out),
        Texticule::TexticuleFrag(JboFragment::JboFragUnparsed(_)) => {}
    }
}

// Rust adaptation: Helper for collect_term_side_texticules_from_texticule
fn collect_term_side_texticules_from_term(term: &JboTerm, out: &mut Vec<Texticule>) {
    match term {
        JboTerm::TermWithSides(inner, sides) => {
            out.extend(sides.iter().cloned());
            collect_term_side_texticules_from_term(inner, out);
        }
        JboTerm::Constant(_, args) => {
            for arg in args {
                collect_term_side_texticules_from_term(arg, out);
            }
        }
        JboTerm::QualifiedTerm(_, inner) => collect_term_side_texticules_from_term(inner, out),
        JboTerm::JoikedTerms(_, left, right) => {
            collect_term_side_texticules_from_term(left, out);
            collect_term_side_texticules_from_term(right, out);
        }
        JboTerm::JboQuote(_) => {}
        _ => {}
    }
}

// Rust adaptation: Helper for collect_term_side_texticules_from_texticule
fn collect_term_side_texticules(prop: &JboProp, out: &mut Vec<Texticule>) {
    match prop {
        Prop::Rel(_, terms) => {
            for term in terms {
                collect_term_side_texticules_from_term(term, out);
            }
        }
        Prop::Not(inner) | Prop::Modal(_, inner) => collect_term_side_texticules(inner, out),
        Prop::Connected(_, left, right) | Prop::NonLogConnected(_, left, right) => {
            collect_term_side_texticules(left, out);
            collect_term_side_texticules(right, out);
        }
        Prop::Quantified(_, _, body) => collect_term_side_texticules(&body(1), out),
        Prop::Eet => {}
    }
}

// Ported from: JboShow.hs :: instance JboShow [Texticule] (jboshow method)
// Includes side texticule collection for Rust's TermWithSides representation
pub fn jboshow_text(text: &[Texticule]) -> String {
    let mut lines = text.iter().map(jboshow_texticule).collect::<Vec<_>>();
    let mut side_lines = Vec::new();
    for texticule in text {
        collect_term_side_texticules_from_texticule(texticule, &mut side_lines);
    }
    let side_lines = side_lines
        .into_iter()
        .flat_map(|texticule| {
            jboshow_texticule(&texticule)
                .lines()
                .filter(|line| !line.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    if !side_lines.is_empty() {
        let insert_at = lines
            .iter()
            .take_while(|line| line.strip_prefix(".i ").unwrap_or(line).starts_with("ju'a nai "))
            .count();
        for (i, line) in side_lines.into_iter().enumerate() {
            lines.insert(insert_at + i, line);
        }
    }
    lines.join("\n.i ")
}

// Ported from: JboShow.hs :: instance JboShow JboTerm (jboshow method)
fn jboshow_term_with_lambdas(term: &JboTerm, lambda_vars: &[i32]) -> String {
    jboshow_term_with_context(term, lambda_vars, None, &[])
}

// Ported from: JboShow.hs :: instance JboShow JboTerm (jboshow method)
// Rust adaptation: explicit context parameters instead of Bindful monad state
fn jboshow_term_with_context(term: &JboTerm, lambda_vars: &[i32], rel_var: Option<ShowBinding>, bindings: &[(i32, ShowBinding)]) -> String {
    match term {
        JboTerm::NonAnaph(s) => s.clone(),
        JboTerm::Named(s) => format!("la {}.", s),
        JboTerm::PredNamed(pred) => {
            let prop = pred(&JboTerm::BoundVar(0));
            format!("la {} ku", jboshow_pred_prop(&prop, 0))
        }
        JboTerm::Constant(n, args) if args.is_empty() => format!("cy {}", jbonum(*n)),
        JboTerm::Constant(n, args) => {
            let arg_strs: Vec<String> = args.iter().map(|t| format!("mo'e {}", jboshow_term_with_context(t, lambda_vars, rel_var, bindings))).collect();
            format!("li ma'o fy{} {} lo'o", jbonum(*n), arg_strs.join(" "))
        }
        JboTerm::Var(_) => "lo NALSELTRO zei da ku".to_string(),
        JboTerm::BoundVar(n) if rel_var == Some(ShowBinding::SRel(*n)) => "ke'a".to_string(),
        JboTerm::BoundVar(n) if lambda_vars.contains(n) => jboshow_lambda_var(*n),
        JboTerm::BoundVar(n) => match term_binding(bindings, *n) {
            Some(ShowBinding::SVar(i)) => jboshow_bound_var(i),
            Some(ShowBinding::SAss(i)) => jboshow_sumti_atom(&SumtiAtom::Assignable(i)),
            Some(ShowBinding::SRel(i)) if i == 1 => "ke'a".to_string(),
            Some(ShowBinding::SRel(i)) => format!("ke'a xi {}", jbonum(i)),
            _ => jboshow_bound_var(*n),
        },
        JboTerm::Unfilled => "BUG".to_string(),
        JboTerm::UnboundSumbasti(atom) => jboshow_sumti_atom(atom),
        JboTerm::Value(m) => format!("li {} lo'o", jboshow_mex_with_context(m, rel_var, bindings)),
        JboTerm::QualifiedTerm(qual, t) => {
            let qualifier = match qual {
                crate::jbo_syntax::SumtiQualifier::LAhE(l) => l.clone(),
                crate::jbo_syntax::SumtiQualifier::NAhE_BO(n) => format!("{} bo", n),
            };
            format!("{} {} lu'u", qualifier, jboshow_term_with_context(t, lambda_vars, rel_var, bindings))
        }
        JboTerm::TheMex(m) => format!("me'o {} lo'o", logshow_syntax_mex(m)),
        JboTerm::JoikedTerms(joik, t1, t2) => {
            let joik = if joik == "??" { "ji" } else { joik };
            format!("{} {} ke {} ke'e", jboshow_term_with_context(t1, lambda_vars, rel_var, bindings), joik, jboshow_term_with_context(t2, lambda_vars, rel_var, bindings))
        }
        JboTerm::JboQuote(text) => format!("lu {} li'u", jboshow_text(text)),
        JboTerm::JboErrorQuote(words) => format!("lo'u {} le'u", words.join(" ")),
        JboTerm::JboNonJboQuote(s) => {
            let delimiter = non_jbo_quote_delimiter(s);
            format!("zoi {} {} {}", delimiter, s, delimiter)
        }
        JboTerm::Valsi(s) => format!("zo {}", s),
        JboTerm::TermWithSides(t, _) => jboshow_term_with_context(t, lambda_vars, rel_var, bindings),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboTerm (jboshow method)
// Wrapper that uses default context
pub fn jboshow_term(term: &JboTerm) -> String {
    jboshow_term_with_lambdas(term, &[])
}

// Ported from: JboShow.hs :: instance JboShow JboTerm (logshow method)
pub fn logshow_term(term: &JboTerm) -> String {
    match term {
        JboTerm::NonAnaph(s) => s.clone(),
        JboTerm::Named(s) => bracket('"', s),
        JboTerm::PredNamed(pred) => {
            let prop = pred(&JboTerm::Named("_".to_string()));
            format!("[Name: {} ]", logshow_prop(&prop).replace("\"_\"", "_"))
        }
        JboTerm::Constant(n, args) if args.is_empty() => format!("c{}", n),
        JboTerm::Constant(n, args) => {
            let arg_strs: Vec<String> = args.iter().map(logshow_term).collect();
            format!("f{}({})", n, arg_strs.join(","))
        }
        JboTerm::Var(_) => "[UNBOUND Var]".to_string(),
        JboTerm::BoundVar(n) if *n < 0 => format!("\\{}", n.abs()),
        JboTerm::BoundVar(n) => format!("x{}", n),
        JboTerm::Unfilled => " ".to_string(),
        JboTerm::UnboundSumbasti(atom) => logshow_sumti_atom(atom),
        JboTerm::Value(m) => logshow_mex(m),
        JboTerm::QualifiedTerm(qual, t) => {
            let qualifier = match qual {
                crate::jbo_syntax::SumtiQualifier::LAhE(l) => l.clone(),
                crate::jbo_syntax::SumtiQualifier::NAhE_BO(n) => format!("{} bo", n),
            };
            format!("{{{}}}({})", qualifier, logshow_term(t))
        }
        JboTerm::TheMex(m) => format!("[MEX: {}]", logshow_syntax_mex(m)),
        JboTerm::JoikedTerms(joik, t1, t2) => format!("({} {{{}}} {})", logshow_term(t1), joik, logshow_term(t2)),
        JboTerm::JboQuote(text) => format!("«{}»", logshow_text(text)),
        JboTerm::JboErrorQuote(words) => format!("<{{< {} >}}>", words.join(" ")),
        JboTerm::JboNonJboQuote(s) => format!("<[< {} >]>", s),
        JboTerm::Valsi(s) => format!("{{{}}}", s),
        JboTerm::TermWithSides(t, _) => logshow_term(t),
    }
}

// Ported from: JboShow.hs :: instance JboShow Connective (jboshow method for LogJboConnective)
fn jboshow_log_connective(prefix: &str, con: &crate::jbo_syntax::LogJboConnective) -> String {
    let mut parts = Vec::new();
    if !con.b1 {
        parts.push("na".to_string());
    }
    if con.c == 'U' {
        parts.push(format!("se {}u", prefix));
    } else {
        parts.push(format!("{}{}", prefix, con.c));
    }
    if !con.b2 {
        parts.push("nai".to_string());
    }
    parts.join(" ")
}

// Ported from: JboShow.hs :: instance JboShow Connective (jboshow method)
fn jboshow_connective(prefix: &str, con: &crate::jbo_syntax::Connective) -> String {
    let (base, mtag) = match con {
        crate::jbo_syntax::Connective::JboConnLog(mtag, lcon) => (jboshow_log_connective(prefix, lcon), mtag.as_deref()),
        crate::jbo_syntax::Connective::JboConnJoik(mtag, joik) => {
            let base = if joik == "??" {
                if prefix == "." { "ji".to_string() } else { "je'i".to_string() }
            } else {
                joik.clone()
            };
            (base, mtag.as_deref())
        }
    };
    if let Some(tag) = mtag {
        format!("{} {} bo", base, jboshow_tag_syntax(tag))
    } else {
        base
    }
}

// Ported from: JboShow.hs :: instance JboShow Tag (jboshow method for syntax-level Tag)
fn jboshow_tag_syntax(tag: &crate::jbo_syntax::Tag) -> String {
    match tag {
        crate::jbo_syntax::Tag::DecoratedTagUnits(dtus) => dtus
            .iter()
            .map(|dtu| match &dtu.tag_unit {
                crate::jbo_syntax::TagUnit::BAI(s)
                | crate::jbo_syntax::TagUnit::TenseCmavo(s)
                | crate::jbo_syntax::TagUnit::CAhA(s)
                | crate::jbo_syntax::TagUnit::CUhE(s) => s.clone(),
                crate::jbo_syntax::TagUnit::FAhA { faha_cmavo, .. } => faha_cmavo.clone(),
                crate::jbo_syntax::TagUnit::TAhE_ZAhO { tahe_zaho_cmavo, .. } => tahe_zaho_cmavo.clone(),
                crate::jbo_syntax::TagUnit::ROI { roiroi, roi_quantifier, .. } => format!("{} {}", jboshow_syntax_mex(roi_quantifier), roiroi),
                crate::jbo_syntax::TagUnit::FIhO(_) => "fi'o".to_string(),
                crate::jbo_syntax::TagUnit::KI => "ki".to_string(),
            })
            .collect::<Vec<_>>()
            .join(" "),
        crate::jbo_syntax::Tag::ConnectedTag(con, tag1, tag2) => format!(
            "{} {} {}",
            jboshow_tag_syntax(tag1),
            jboshow_connective("j", con),
            jboshow_tag_syntax(tag2)
        ),
    }
}

// Ported from: JboShow.hs :: instance JboShow Connective (logshow method)
fn logshow_connective(prefix: &str, con: &crate::jbo_syntax::Connective) -> String {
    format!("{{{}}}", jboshow_connective(prefix, con))
}

// Ported from: JboShow.hs :: instance JboShow JboTag (jboshow method for semantic JboTag)
pub fn jboshow_tag(tag: &crate::jbo_prop::JboTag) -> String {
    match tag {
        crate::jbo_prop::JboTag::DecoratedTagUnits(dtus) => dtus
            .iter()
            .map(|dtu| {
                let unit = match &dtu.tag_unit {
                    crate::jbo_prop::JboTagUnit::BAI(s)
                    | crate::jbo_prop::JboTagUnit::TenseCmavo(s)
                    | crate::jbo_prop::JboTagUnit::CAhA(s)
                    | crate::jbo_prop::JboTagUnit::CUhE(s) => s.clone(),
                    crate::jbo_prop::JboTagUnit::FAhA(Some(true), s) => format!("mo'i {}", s),
                    crate::jbo_prop::JboTagUnit::FAhA(_, s) => s.clone(),
                    crate::jbo_prop::JboTagUnit::TAhE_ZAhO(true, s) => format!("fe'e {}", s),
                    crate::jbo_prop::JboTagUnit::TAhE_ZAhO(_, s) => s.clone(),
                    crate::jbo_prop::JboTagUnit::ROI(s, true, q) => format!("fe'e {} {}", jboshow_mex_number(q), s),
                    crate::jbo_prop::JboTagUnit::ROI(s, _, q) => format!("{} {}", jboshow_mex_number(q), s),
                    crate::jbo_prop::JboTagUnit::FIhO(p) => format!("fi'o {} fe'u", jboshow_pred_prop(&p(&[crate::jbo_prop::JboTerm::BoundVar(0)]), 0)),
                    crate::jbo_prop::JboTagUnit::KI => "ki".to_string(),
                };
                let decorated = format!(
                    "{}{}{}{}",
                    dtu.nahe.as_ref().map(|s| format!("{} ", s)).unwrap_or_default(),
                    dtu.se.map(|se| format!("{} ", se_to_str(se))).unwrap_or_default(),
                    unit,
                    if dtu.nai { " nai" } else { "" }
                );
                decorated
            })
            .collect::<Vec<_>>()
            .join(" "),
        crate::jbo_prop::JboTag::ConnectedTag(con, tag1, tag2) => {
            let con = match con.as_ref() {
                crate::jbo_prop::JboConnective::JboConnLog(_, lcon) => jboshow_log_connective("j", lcon),
                crate::jbo_prop::JboConnective::JboConnJoik(_, joik) => {
                    if joik == "??" { "je'i".to_string() } else { joik.clone() }
                }
            };
            format!("{} {} {}", jboshow_tag(tag1), con, jboshow_tag(tag2))
        }
    }
}

// Ported from: JboShow.hs :: instance JboShow JboModalOp (logshow method)
// Helper for modal prefix in logical output
fn logshow_modal_rel_prefix(modal: &JboModalOp, placeholder_var: Option<i32>) -> String {
    match modal {
        JboModalOp::Tagged(tag, term) => {
            let term = term.as_ref().map(|term| {
                if matches!(placeholder_var, Some(var) if matches!(term, JboTerm::BoundVar(n) if *n == var)) {
                    "_".to_string()
                } else {
                    logshow_term(term)
                }
            }).unwrap_or_default();
            format!("({})({})", jboshow_tag(tag), term)
        }
        _ => format!("Modal({:?})", modal),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboModalOp (logshow method)
// Full modal relation display for logical output
fn logshow_modal_rel(modal: &JboModalOp, rel: &JboRel) -> String {
    format!("{}. {}(_)", logshow_modal_rel_prefix(modal, None), logshow_rel(rel))
}

// Ported from: JboShow.hs :: instance JboShow JboModalOp (jboshow method)
// Modal relation display for canonical Lojban output
fn jboshow_modal_rel(modal: &JboModalOp, rel: &JboRel, lambda_vars: &[i32], rel_var: Option<ShowBinding>, bindings: &[(i32, ShowBinding)]) -> String {
    match modal {
        JboModalOp::Tagged(tag, term) => {
            let tag = jboshow_tag(tag);
            let term = term
                .as_ref()
                .map(|t| format!(" {}", jboshow_term_with_context(t, lambda_vars, rel_var, bindings)))
                .unwrap_or_default();
            format!("poi'i {}{} zo'u ke'a {} kei", tag, term, jboshow_rel_with_context(rel, lambda_vars, rel_var, bindings))
        }
        _ => format!("poi'i {} kei", jboshow_rel_with_context(rel, lambda_vars, rel_var, bindings)),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboRel (jboshow method)
// Wrapper that uses default lambda_vars context
fn jboshow_rel_with_lambdas(rel: &JboRel, lambda_vars: &[i32]) -> String {
    jboshow_rel_with_context(rel, lambda_vars, None, &[])
}

// Ported from: JboShow.hs :: logjboshow' Rel (scalar negation case)
// Mirrors scalar NAhE display after parsedSelbriToNewSelbri creates applied rel with skipped x2
fn jboshow_scalar_negated_rel_inner(rel: &JboRel, lambda_vars: &[i32], rel_var: Option<ShowBinding>, bindings: &[(i32, ShowBinding)]) -> String {
    match rel {
        JboRel::AppliedRel(r, terms) if terms.len() > 2 && matches!(terms.get(1), Some(JboTerm::Unfilled)) => {
            let terms = std::iter::once(terms[0].clone())
                .chain(terms.iter().skip(2).cloned())
                .collect::<Vec<_>>();
            jboshow_rel_with_context(&JboRel::AppliedRel(r.clone(), terms), lambda_vars, rel_var, bindings)
        }
        _ => jboshow_rel_with_context(rel, lambda_vars, rel_var, bindings),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboRel (jboshow method)
// Rust adaptation: explicit context parameters instead of Bindful monad state
fn jboshow_rel_with_context(rel: &JboRel, lambda_vars: &[i32], rel_var: Option<ShowBinding>, bindings: &[(i32, ShowBinding)]) -> String {
    match rel {
        JboRel::Brivla(s) => s.clone(),
        JboRel::Equal => "du".to_string(),
        JboRel::Tanru(r1, r2) => {
            format!("ke {} {} ke'e", jboshow_rel_with_context(r1, lambda_vars, rel_var, bindings), jboshow_rel_with_context(r2, lambda_vars, rel_var, bindings))
        }
        JboRel::AppliedRel(r, terms) => {
            let placeholder = terms.iter().position(|term| matches!(term, JboTerm::BoundVar(0)));
            let mut out = if let Some(idx) = placeholder {
                if idx == 0 {
                    jboshow_rel_with_context(r, lambda_vars, rel_var, bindings)
                } else {
                    format!("{} {}", se_to_str((idx + 1) as i32), jboshow_rel_with_context(r, lambda_vars, rel_var, bindings))
                }
            } else {
                jboshow_rel_with_context(r, lambda_vars, rel_var, bindings)
            };

            let mut rest: Vec<(usize, &JboTerm)> = match placeholder {
                Some(0) => terms.iter().enumerate().skip(1).collect(),
                Some(idx) => terms.iter().enumerate().filter(|(i, _)| *i != idx).collect(),
                None => terms.iter().enumerate().skip(1).collect(),
            };
            if let Some(idx) = placeholder {
                if idx > 0 {
                    rest.sort_by_key(|(i, _)| if *i == 0 { idx } else { *i });
                }
            }
            let mut first_link = true;
            let mut previous_unfilled = false;

            for (_idx, term) in rest {
                if matches!(term, JboTerm::Unfilled) {
                    previous_unfilled = true;
                    continue;
                }

                let marker = if first_link { "be" } else { "bei" };
                if previous_unfilled {
                    let place = (_idx + 1) as i32;
                    out.push_str(&format!(" {} {} {}", marker, fa_to_str(place), jboshow_term_with_context(term, lambda_vars, rel_var, bindings)));
                } else {
                    out.push_str(&format!(" {} {}", marker, jboshow_term_with_context(term, lambda_vars, rel_var, bindings)));
                }
                first_link = false;
                previous_unfilled = false;
            }
            out
        }
        JboRel::TanruConnective(con, r1, r2) => {
            format!("ke {} ke'e {} ke {} ke'e", jboshow_rel_with_context(r1, lambda_vars, rel_var, bindings), jboshow_connective("j", con), jboshow_rel_with_context(r2, lambda_vars, rel_var, bindings))
        }
        JboRel::PermutedRel(n, r) => format!("{} {}", se_to_str(*n), jboshow_rel_with_context(r, lambda_vars, rel_var, bindings)),
        JboRel::Among(t) => format!("me {} me'u", jboshow_term_with_context(t, lambda_vars, rel_var, bindings)),
        JboRel::RVar(_) => "NALSELTRO zei bu'a".to_string(),
        JboRel::BoundRVar(n) => match rel_var {
            Some(ShowBinding::SRVar(rv)) if rv == *n => jboshow_unassigned_rvar(*n),
            Some(ShowBinding::SRAss(rv)) if rv == *n => jboshow_bound_rvar(*n),
            _ => format!("bu'a xi {}", jbonum(*n)),
        },
        JboRel::RAss(n) if rel_var == Some(ShowBinding::SRAss(*n)) => jboshow_bound_rvar(*n),
        JboRel::RAss(n) => format!("broda xi {}", jbonum(*n)),
        JboRel::UnboundBribasti(tu) => match tu {
            crate::jbo_syntax::TanruUnit::TUGOhA(g, n) => {
                if *n == 1 {
                    g.clone()
                } else {
                    format!("{} xi {}", g, jbonum(*n))
                }
            }
            crate::jbo_syntax::TanruUnit::TUBrivla(s) => s.clone(),
            _ => format!("UNBOUND({:?})", tu),
        },
        JboRel::Moi(t, moi) => match t.as_ref() {
            JboTerm::Value(m @ (JboMex::MexInt(_) | JboMex::MexNumeralString(_) | JboMex::MexLerfuString(_))) => format!("{} {}", jboshow_mex_number(m), moi),
            _ => format!("me {} me'u {}", jboshow_term_with_context(t, lambda_vars, rel_var, bindings), moi),
        },
        JboRel::OperatorRel(op) => jbobracket("nu'a", "te'u", &jboshow_operator_with_context(op, rel_var, bindings)),
        JboRel::ScalarNegatedRel(nahe, r) => format!("{} {}", nahe, jboshow_scalar_negated_rel_inner(r, lambda_vars, rel_var, bindings)),
        JboRel::VPredRel(vpred) => jboshow_pred_prop_with_rel_context(&vpred(&[JboTerm::BoundVar(0)]), 0, rel_var),
        JboRel::AbsPred(abs, npred) => {
            let args: Vec<JboTerm> = (1..=npred.arity as i32).map(|n| JboTerm::BoundVar(-n)).collect();
            let nested_lambda_vars = args.iter().filter_map(|term| match term { JboTerm::BoundVar(n) => Some(*n), _ => None }).collect::<Vec<_>>();
            format!("{} {} kei", abs, jboshow_prop_with_context_at(&(npred.pred)(&args), &nested_lambda_vars, rel_var, next_binding_key(bindings), bindings))
        }
        JboRel::AbsProp(abs, prop) => format!("{} {} kei", abs, jboshow_prop_with_context_at(prop, lambda_vars, rel_var, next_binding_key(bindings), bindings)),
        JboRel::TagRel(tag) => format!("xo'i {}", jboshow_tag(tag)),
        JboRel::ModalRel(modal, r) => jboshow_modal_rel(modal, r, lambda_vars, rel_var, bindings),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboRel (jboshow method)
// Wrapper that uses default context
pub fn jboshow_rel(rel: &JboRel) -> String {
    jboshow_rel_with_lambdas(rel, &[])
}

// Ported from: JboShow.hs :: instance JboShow JboTerm (logshow method for lambda terms)
fn logshow_lambda_term(term: &JboTerm) -> String {
    match term {
        JboTerm::BoundVar(n) if *n < 0 => format!("\\{}", n.abs()),
        _ => logshow_term(term),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboRel (logshow method for lambda relations)
fn logshow_lambda_rel(rel: &JboRel) -> String {
    match rel {
        JboRel::AppliedRel(r, terms) => {
            format!("{}({})", logshow_rel(r), terms.iter().map(logshow_lambda_term).collect::<Vec<_>>().join(","))
        }
        _ => logshow_rel(rel),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboTerm (logshow method for lambda restriction terms)
fn logshow_lambda_restriction_term(term: &JboTerm, var: i32) -> String {
    match term {
        JboTerm::BoundVar(n) if *n == var => "_".to_string(),
        _ => logshow_lambda_term(term),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (logshow method for lambda restriction propositions)
fn logshow_lambda_restriction_prop(prop: &JboProp, var: i32) -> String {
    match prop {
        Prop::Rel(JboRel::Among(t), terms) if matches!(terms.as_slice(), [JboTerm::BoundVar(n)] if *n == var) => {
            format!("(_ ≤ {})", logshow_lambda_restriction_term(t, var))
        }
        Prop::Rel(JboRel::AppliedRel(r, applied_terms), terms)
            if terms.is_empty()
                && matches!(r.as_ref(), JboRel::Among(_))
                && matches!(applied_terms.as_slice(), [JboTerm::BoundVar(n)] if *n == var) =>
        {
            if let JboRel::Among(t) = r.as_ref() {
                format!("(_ ≤ {})", logshow_lambda_restriction_term(t, var))
            } else {
                unreachable!()
            }
        }
        Prop::Rel(rel, terms) => {
            format!("{}({})", logshow_lambda_rel(rel), terms.iter().map(|t| logshow_lambda_restriction_term(t, var)).collect::<Vec<_>>().join(","))
        }
        _ => logshow_lambda_prop(prop),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (logshow method for lambda propositions with placeholder)
fn logshow_lambda_prop_with_placeholder(prop: &JboProp, placeholder_var: Option<i32>) -> String {
    match prop {
        Prop::Rel(rel, terms) => {
            let term_strs: Vec<String> = terms.iter().map(|term| {
                if matches!(placeholder_var, Some(var) if matches!(term, JboTerm::BoundVar(n) if *n == var)) {
                    "_".to_string()
                } else {
                    logshow_lambda_term(term)
                }
            }).collect();
            format!("{}({})", logshow_lambda_rel(rel), term_strs.join(","))
        }
        Prop::Modal(JboModalOp::Tagged(tag, mt), inner) => {
            if let Some(expanded) = expand_connected_tag_modal(tag, mt, inner) {
                return logshow_lambda_prop_with_placeholder(&expanded, placeholder_var);
            }
            let term = mt.as_ref().map(|term| {
                if matches!(placeholder_var, Some(var) if matches!(term, JboTerm::BoundVar(n) if *n == var)) {
                    "_".to_string()
                } else {
                    logshow_lambda_term(term)
                }
            }).unwrap_or_default();
            format!("({})({}). {}", jboshow_tag(tag), term, logshow_lambda_prop_with_placeholder(inner, placeholder_var))
        }
        Prop::Modal(JboModalOp::WithEventAs(term), inner) => {
            let term = if matches!(placeholder_var, Some(var) if matches!(term, JboTerm::BoundVar(n) if *n == var)) {
                "_".to_string()
            } else {
                logshow_lambda_term(term)
            };
            format!("{}=. {}", term, logshow_lambda_prop_with_placeholder(inner, placeholder_var))
        }
        Prop::Modal(JboModalOp::QTruthModal, inner) => format!("?. {}", logshow_lambda_prop_with_placeholder(inner, placeholder_var)),
        Prop::Modal(JboModalOp::NonVeridical, inner) => format!("non-veridical: {}", logshow_lambda_prop_with_placeholder(inner, placeholder_var)),
        Prop::Not(inner) => format!("¬{}", logshow_lambda_prop_with_placeholder(inner, placeholder_var)),
        Prop::Connected(con, p1, p2) => {
            format!("({} {} {})", logshow_lambda_prop_with_placeholder(p1, placeholder_var), con, logshow_lambda_prop_with_placeholder(p2, placeholder_var))
        }
        Prop::NonLogConnected(joik, p1, p2) => {
            format!("({} {{{}}} {})", logshow_lambda_prop_with_placeholder(p1, placeholder_var), joik, logshow_lambda_prop_with_placeholder(p2, placeholder_var))
        }
        Prop::Quantified(q, restriction, body) => {
            let n = 1;
            let mut prefix = match q {
                JboQuantifier::RelQuantifier(inner_q) => format_quantified_prefix(inner_q, &format!("R{}", n)),
                _ => format_quantified_prefix(q, &format!("x{}", n)),
            };
            if let Some(r) = restriction {
                prefix.push_str(":(");
                prefix.push_str(&logshow_lambda_restriction_prop(&r(n), n));
                prefix.push_str("). ");
            } else {
                prefix.push_str(". ");
            }
            format!("{}{}", prefix, logshow_lambda_prop_with_placeholder(&body(n), placeholder_var))
        }
        Prop::Eet => "_|_ (BUG)".to_string(),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (logshow method for lambda propositions)
fn logshow_lambda_prop(prop: &JboProp) -> String {
    logshow_lambda_prop_with_placeholder(prop, None)
}

// Ported from: JboShow.hs :: instance JboShow JboRel (logshow method)
pub fn logshow_rel(rel: &JboRel) -> String {
    match rel {
        JboRel::Brivla(s) => s.clone(),
        JboRel::Equal => "=".to_string(),
        JboRel::Tanru(r1, r2) => {
            // Ported from: JboShow.hs lines 335-340
            // In Haskell, seltau is a VPred applied to BoundVar→SRel 1→"_"
            // producing e.g. "sutra(_)". Since Rust stores Box<JboRel>, we
            // synthesize (_) by formatting seltau with placeholder.
            let seltau = logshow_seltau(r1);
            format!("<{}><{}>", seltau, logshow_rel(r2))
        }
        JboRel::AppliedRel(r, terms) => {
            format!("{}({})", logshow_rel(r), terms.iter().map(logshow_term).collect::<Vec<_>>().join(","))
        }
        JboRel::TanruConnective(con, r1, r2) => {
            format!("<{}>{}<{}>", logshow_seltau(r1), logshow_connective("j", con), logshow_seltau(r2))
        }
        JboRel::PermutedRel(n, r) => format!("{} {}", se_to_str(*n), logshow_rel(r)),
        JboRel::Among(t) => format!("({} ≥ ", logshow_term(t)),
        JboRel::RVar(_) => "[UNBOUND RVar]".to_string(),
        JboRel::BoundRVar(n) => format!("R{}", n),
        JboRel::RAss(n) => format!("R{}", n),
        JboRel::UnboundBribasti(tu) => format!("[UNBOUND {:?}]", tu),
        JboRel::Moi(t, moi) => match t.as_ref() {
            JboTerm::Value(JboMex::MexInt(_) | JboMex::MexNumeralString(_) | JboMex::MexLerfuString(_)) => format!("{} {}", logshow_term(t), moi),
            _ => format!("[{}] {}", logshow_term(t), moi),
        },
        JboRel::OperatorRel(op) => format!("[{}]", logshow_operator(op)),
        JboRel::ScalarNegatedRel(nahe, r) => format!("{{{}}}({})", nahe, logshow_rel(r)),
        JboRel::VPredRel(vpred) => logshow_prop(&vpred(&[JboTerm::BoundVar(0)])),
        JboRel::AbsPred(abs, npred) => {
            let args: Vec<JboTerm> = (1..=npred.arity as i32).map(|n| JboTerm::BoundVar(-n)).collect();
            format!("{}[{}]", abs, logshow_lambda_prop(&(npred.pred)(&args)))
        }
        JboRel::AbsProp(abs, prop) => format!("{}[{}]", abs, logshow_prop(prop)),
        JboRel::TagRel(tag) => format!("{{{}}}", jboshow_tag(tag)),
        JboRel::ModalRel(modal, r) => logshow_modal_rel(modal, r),
    }
}

// Ported from: JboShow.hs :: logjboshow for JboVPred with Bindful.hs :: withShuntedRelVar
// Formats seltau (tanru left component) with placeholder variable for logical output
pub fn logshow_seltau(rel: &JboRel) -> String {
    match rel {
        JboRel::VPredRel(vpred) => match vpred(&[JboTerm::BoundVar(0)]) {
            Prop::Rel(JboRel::Among(t), terms) if terms.is_empty() => format!("(_ <= {})", logshow_term(&t)),
            Prop::Rel(JboRel::AppliedRel(r, applied_terms), terms)
                if terms.is_empty()
                    && matches!(r.as_ref(), JboRel::Among(_))
                    && matches!(applied_terms.as_slice(), [JboTerm::BoundVar(0)]) =>
            {
                if let JboRel::Among(t) = r.as_ref() {
                    format!("(_ <= {})", logshow_term(t))
                } else {
                    unreachable!()
                }
            }
            Prop::Rel(rel, terms) if terms.is_empty() => format!("{}(_)", logshow_rel(&rel)),
            prop => logshow_lambda_prop_with_placeholder(&prop, Some(0)),
        },
        JboRel::AppliedRel(r, terms) if matches!(r.as_ref(), JboRel::Among(_)) => {
            if let JboRel::Among(t) = r.as_ref() {
                format!("(_ <= {})", logshow_term(t))
            } else {
                unreachable!()
            }
        }
        JboRel::AppliedRel(r, terms) => {
            let term_strs = terms.iter().map(|term| {
                if matches!(term, JboTerm::BoundVar(0)) {
                    "_".to_string()
                } else {
                    logshow_term(term)
                }
            }).collect::<Vec<_>>().join(",");
            match r.as_ref() {
                JboRel::ModalRel(modal, inner) => format!("{}. {}({})", logshow_modal_rel_prefix(modal, Some(0)), logshow_rel(inner), term_strs),
                _ => format!("{}({})", logshow_rel(r), term_strs),
            }
        }
        JboRel::Tanru(_, _) => {
            // Nested tanru: format as tanru, then append (_)
            // e.g. Tanru(sutra, tavla) → "<sutra(_)><tavla>(_)"
            format!("{}(_)", logshow_rel(rel))
        }
        _ => {
            // Simple brivla: append (_) directly
            // e.g. sutra → "sutra(_)"
            format!("{}(_)", logshow_rel(rel))
        }
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (jboshow method)
// Wrapper that uses default lambda_vars context
fn jboshow_prop_with_lambdas(prop: &JboProp, lambda_vars: &[i32]) -> String {
    jboshow_prop_with_context(prop, lambda_vars, None)
}

// Ported from: JboShow.hs :: jboshowPred / positionallyTaggedTerms
// Formats predicate propositions for abstraction display
fn jboshow_pred_prop(prop: &JboProp, rel_var: i32) -> String {
    jboshow_pred_prop_with_rel_context(prop, rel_var, Some(ShowBinding::SRel(rel_var)))
}

// Ported from: JboShow.hs :: jboshowPred / positionallyTaggedTerms
// Formats predicate propositions with explicit rel_var context
fn jboshow_pred_prop_with_rel_context(prop: &JboProp, placeholder_var: i32, active_rel_var: Option<ShowBinding>) -> String {
    match prop {
        Prop::Rel(rel, terms) if terms.is_empty() => {
            jboshow_rel_with_context(rel, &[], active_rel_var, &[])
        }
        Prop::Rel(rel, terms) => {
            if let Some(idx) = terms.iter().position(|term| matches!(term, JboTerm::BoundVar(n) if *n == placeholder_var)) {
                let rel_str = if idx == 0 {
                    jboshow_rel_with_context(rel, &[], active_rel_var, &[])
                } else {
                    format!("{} {}", se_to_str((idx + 1) as i32), jboshow_rel_with_context(rel, &[], active_rel_var, &[]))
                };
                let mut rest_terms: Vec<(usize, &JboTerm)> = if idx == 0 {
                    terms.iter().enumerate().skip(1).collect()
                } else {
                    terms.iter().enumerate().filter(|(i, _)| *i != idx).collect()
                };
                if idx > 0 {
                    rest_terms.sort_by_key(|(i, _)| if *i == 0 { idx } else { *i });
                }
                let mut rest = Vec::new();
                let mut previous_unfilled = false;
                for (term_idx, term) in rest_terms {
                    if matches!(term, JboTerm::Unfilled) {
                        previous_unfilled = true;
                        continue;
                    }
                    let term_str = jboshow_term_with_context(term, &[], active_rel_var, &[]);
                    if previous_unfilled {
                        rest.push(format!("{} {}", fa_to_str((term_idx + 1) as i32), term_str));
                    } else {
                        rest.push(term_str);
                    }
                    previous_unfilled = false;
                }
                if rest.is_empty() {
                    rel_str
                } else {
                    format!("{} be {}", rel_str, rest.join(" bei "))
                }
            } else {
                format!("poi'i {} kei", jboshow_prop_with_context(prop, &[], Some(ShowBinding::SRel(placeholder_var))))
            }
        }
        _ => format!("poi'i {} kei", jboshow_prop_with_context(prop, &[], Some(ShowBinding::SRel(placeholder_var)))),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (jboshow method)
// Rust adaptation: explicit binding state threading instead of Bindful monad
fn jboshow_prop_with_context(prop: &JboProp, lambda_vars: &[i32], rel_var: Option<ShowBinding>) -> String {
    jboshow_prop_with_context_at(prop, lambda_vars, rel_var, 1, &[])
}

// Rust-only: Helper for finding next available binding index
fn next_binding_index(bindings: &[(i32, ShowBinding)], binding: impl Fn(i32) -> ShowBinding) -> i32 {
    (1..)
        .find(|n| !bindings.iter().any(|(_, existing)| *existing == binding(*n)))
        .unwrap()
}

// Rust-only: Helper for adding a binding to the binding list
fn with_binding(bindings: &[(i32, ShowBinding)], key: i32, binding: ShowBinding) -> Vec<(i32, ShowBinding)> {
    let mut next = bindings.to_vec();
    next.push((key, binding));
    next
}

// Rust-only: Helper for looking up a term binding by key
fn term_binding(bindings: &[(i32, ShowBinding)], key: i32) -> Option<ShowBinding> {
    bindings.iter().rev().find_map(|(binding_key, binding)| (*binding_key == key).then_some(*binding))
}

// Rust-only: Helper for finding next available binding key
fn next_binding_key(bindings: &[(i32, ShowBinding)]) -> i32 {
    (1..).find(|n| !bindings.iter().any(|(key, _)| key == n)).unwrap()
}

// Ported from: JboShow.hs :: jboshow' Prop (modal prefix accumulation)
fn jboshow_modal_prefix(modal_op: &JboModalOp, lambda_vars: &[i32], rel_var: Option<ShowBinding>, bindings: &[(i32, ShowBinding)]) -> Vec<String> {
    match modal_op {
        JboModalOp::Tagged(tag, mt) => {
            let mut parts = vec![jboshow_tag(tag)];
            parts.push(mt.as_ref()
                .map(|t| jboshow_term_with_context(t, lambda_vars, rel_var, bindings))
                .unwrap_or_else(|| "ku".to_string()));
            parts
        }
        JboModalOp::WithEventAs(t) => vec!["fi'o".to_string(), "du".to_string(), jboshow_term_with_context(t, lambda_vars, rel_var, bindings)],
        JboModalOp::QTruthModal => vec!["xu".to_string(), "kau".to_string()],
        JboModalOp::NonVeridical => Vec::new(),
    }
}

// Ported from: JboShow.hs :: jboshow' Prop (prefix threading for modals and quantifiers)
fn jboshow_prop_with_prefixes(prop: &JboProp, lambda_vars: &[i32], rel_var: Option<ShowBinding>, next_var: i32, bindings: &[(i32, ShowBinding)], mut prefixes: Vec<String>) -> String {
    match prop {
        Prop::Modal(JboModalOp::NonVeridical, inner) => {
            format!("ju'a nai {}", jboshow_prop_with_prefixes(inner, lambda_vars, rel_var, next_var, bindings, prefixes))
        }
        Prop::Modal(modal_op, inner) => {
            if let JboModalOp::Tagged(tag, mt) = modal_op {
                if let Some(expanded) = expand_connected_tag_modal(tag, mt, inner) {
                    return jboshow_prop_with_prefixes(&expanded, lambda_vars, rel_var, next_var, bindings, prefixes);
                }
            }
            prefixes.extend(jboshow_modal_prefix(modal_op, lambda_vars, rel_var, bindings));
            jboshow_prop_with_prefixes(inner, lambda_vars, rel_var, next_var, bindings, prefixes)
        }
        Prop::Quantified(JboQuantifier::QuestionQuantifier, restriction, body) => {
            let key = next_var;
            let n = next_binding_index(bindings, ShowBinding::SAss);
            let question_bindings = with_binding(bindings, key, ShowBinding::SAss(n));
            prefixes.push("ma".to_string());
            if let Some(r) = restriction {
                prefixes.push("poi".to_string());
                prefixes.push(jboshow_prop_with_context_at(&r(key), lambda_vars, rel_var, key + 1, &question_bindings));
                prefixes.push("ku'o".to_string());
                prefixes.push("zi'e".to_string());
            }
            prefixes.push("goi".to_string());
            prefixes.push(jboshow_sumti_atom(&SumtiAtom::Assignable(n)));
            jboshow_prop_with_prefixes(&body(key), lambda_vars, rel_var, key + 1, &question_bindings, prefixes)
        }
        Prop::Quantified(JboQuantifier::RelQuantifier(inner_q), _, body)
            if matches!(inner_q.as_ref(), JboQuantifier::QuestionQuantifier) =>
        {
            let key = 1;
            let n = next_binding_index(bindings, ShowBinding::SRAss);
            let rel_bindings = with_binding(bindings, key, ShowBinding::SRAss(n));
            prefixes.extend(["mo".to_string(), "cei".to_string(), jboshow_bound_rvar(n)]);
            jboshow_prop_with_prefixes(&body(key), lambda_vars, Some(ShowBinding::SRAss(n)), next_var, &rel_bindings, prefixes)
        }
        Prop::Quantified(JboQuantifier::RelQuantifier(inner_q), _, body) => {
            let key = 1;
            let n = next_binding_index(bindings, ShowBinding::SRVar);
            let rel_bindings = with_binding(bindings, key, ShowBinding::SRVar(n));
            prefixes.push(jboshow_quantifier(inner_q));
            prefixes.push(jboshow_unassigned_rvar(n));
            jboshow_prop_with_prefixes(&body(key), lambda_vars, Some(ShowBinding::SRVar(n)), next_var, &rel_bindings, prefixes)
        }
        Prop::Quantified(q, restriction, body) => {
            let key = next_var;
            let n = next_binding_index(bindings, ShowBinding::SVar);
            let body_bindings = with_binding(bindings, key, ShowBinding::SVar(n));
            let mut parts = prefixes;
            parts.push(jboshow_quantifier(q));
            parts.push(jboshow_bound_var(n));
            if let Some(r) = restriction {
                let rel_bindings = with_binding(&body_bindings, key, ShowBinding::SRel(1));
                parts.push("poi".to_string());
                parts.push(jboshow_prop_with_context_at(&r(key), lambda_vars, Some(ShowBinding::SRel(key)), key + 1, &rel_bindings));
                parts.push("ku'o".to_string());
            }
            let body_prop = body(key);
            if matches!(body_prop, Prop::Quantified(_, _, _)) {
                parts.push(jboshow_prop_with_context_at(&body_prop, lambda_vars, rel_var, key + 1, &body_bindings));
                parts.join(" ")
            } else {
                jboshow_prop_with_prefixes(&body_prop, lambda_vars, rel_var, key + 1, &body_bindings, parts)
            }
        }
        _ if prefixes.is_empty() => jboshow_prop_with_context_at(prop, lambda_vars, rel_var, next_var, bindings),
        _ => format!("{} zo'u {}", prefixes.join(" "), jboshow_prop_with_context_at(prop, lambda_vars, rel_var, next_var, bindings)),
    }
}

// Ported from: JboShow.hs :: positionallyTaggedTerms (scalar negation term reordering)
fn scalar_negated_terms(terms: &[JboTerm]) -> Vec<JboTerm> {
    if terms.len() > 3 && matches!(terms.get(1), Some(JboTerm::Unfilled)) {
        std::iter::once(terms[0].clone())
            .chain(terms.iter().skip(3).cloned())
            .chain(std::iter::once(terms[2].clone()))
            .collect()
    } else {
        terms.to_vec()
    }
}

// Ported from: JboShow.hs :: jboshow' Prop (main proposition formatting with binding context)
fn jboshow_prop_with_context_at(prop: &JboProp, lambda_vars: &[i32], rel_var: Option<ShowBinding>, next_var: i32, bindings: &[(i32, ShowBinding)]) -> String {
    match prop {
        Prop::Rel(rel, terms) => {
            let terms = if matches!(rel, JboRel::ScalarNegatedRel(_, _)) { scalar_negated_terms(terms) } else { terms.clone() };
            let rel_str = jboshow_rel_with_context(rel, lambda_vars, rel_var, bindings);
            if terms.is_empty() {
                rel_str
            } else if terms.len() == 1 {
                if matches!(terms[0], JboTerm::Unfilled) {
                    rel_str
                } else {
                    format!("{} {}", jboshow_term_with_context(&terms[0], lambda_vars, rel_var, bindings), rel_str)
                }
            } else if matches!(terms[0], JboTerm::Unfilled) {
                let mut rest = Vec::new();
                let mut previous_unfilled = false;
                for (idx, term) in terms.iter().enumerate().skip(1) {
                    if matches!(term, JboTerm::Unfilled) {
                        previous_unfilled = true;
                        continue;
                    }
                    let term_str = jboshow_term_with_context(term, lambda_vars, rel_var, bindings);
                    if previous_unfilled {
                        rest.push(format!("{} {}", fa_to_str((idx + 1) as i32), term_str));
                    } else {
                        rest.push(term_str);
                    }
                    previous_unfilled = false;
                }
                if rest.is_empty() {
                    rel_str
                } else {
                    format!("{} {}", rel_str, rest.join(" "))
                }
            } else {
                let x1 = jboshow_term_with_context(&terms[0], lambda_vars, rel_var, bindings);
                let mut rest = Vec::new();
                let mut previous_unfilled = false;
                for (idx, term) in terms.iter().enumerate().skip(1) {
                    if matches!(term, JboTerm::Unfilled) {
                        previous_unfilled = true;
                        continue;
                    }
                    let term_str = jboshow_term_with_context(term, lambda_vars, rel_var, bindings);
                    if previous_unfilled {
                        rest.push(format!("{} {}", fa_to_str((idx + 1) as i32), term_str));
                    } else {
                        rest.push(term_str);
                    }
                    previous_unfilled = false;
                }
                if rest.is_empty() {
                    format!("{} {}", x1, rel_str)
                } else {
                    format!("{} {} {}", x1, rel_str, rest.join(" "))
                }
            }
        }
        Prop::Connected(conn, p1, p2) => {
            let ss1 = jboshow_prop_with_context_at(p1, lambda_vars, rel_var, next_var, bindings);
            let ss2 = jboshow_prop_with_context_at(p2, lambda_vars, rel_var, next_var, bindings);
            let prefix = match conn {
                crate::logic::Connective::And => "ge".to_string(),
                crate::logic::Connective::Or => "ga".to_string(),
                crate::logic::Connective::Impl => "ga nai".to_string(),
                crate::logic::Connective::Equiv => "go".to_string(),
            };
            format!("{} {} gi {}", prefix, ss1, ss2)
        }
        Prop::NonLogConnected(joik, p1, p2) => {
            let ss1 = jboshow_prop_with_context_at(p1, lambda_vars, rel_var, next_var, bindings);
            let ss2 = jboshow_prop_with_context_at(p2, lambda_vars, rel_var, next_var, bindings);
            if joik == "??" {
                format!("ge'i {} gi {}", ss1, ss2)
            } else {
                format!("{} gi {} gi {}", joik, ss1, ss2)
            }
        }
        Prop::Modal(_, _) => jboshow_prop_with_prefixes(prop, lambda_vars, rel_var, next_var, bindings, Vec::new()),
        Prop::Not(inner) => format!("na ku {}", jboshow_prop_with_context_at(inner, lambda_vars, rel_var, next_var, bindings)),
        Prop::Quantified(JboQuantifier::RelQuantifier(_), _, _) => {
            jboshow_prop_with_prefixes(prop, lambda_vars, rel_var, next_var, bindings, Vec::new())
        }
        Prop::Quantified(JboQuantifier::QuestionQuantifier, _, _) => {
            jboshow_prop_with_prefixes(prop, lambda_vars, rel_var, next_var, bindings, Vec::new())
        }
        Prop::Quantified(q, restriction, body) => {
            let key = next_var;
            let n = next_binding_index(bindings, ShowBinding::SVar);
            let body_bindings = with_binding(bindings, key, ShowBinding::SVar(n));
            let mut parts = vec![jboshow_quantifier(q), jboshow_bound_var(n)];
            if let Some(r) = restriction {
                let rel_bindings = with_binding(&body_bindings, key, ShowBinding::SRel(1));
                parts.push("poi".to_string());
                parts.push(jboshow_prop_with_context_at(&r(key), lambda_vars, Some(ShowBinding::SRel(key)), key + 1, &rel_bindings));
                parts.push("ku'o".to_string());
            }
            let body_prop = body(key);
            if matches!(body_prop, Prop::Quantified(_, _, _)) {
                parts.push(jboshow_prop_with_context_at(&body_prop, lambda_vars, rel_var, key + 1, &body_bindings));
            } else {
                return jboshow_prop_with_prefixes(&body_prop, lambda_vars, rel_var, key + 1, &body_bindings, parts);
            }
            parts.join(" ")
        }
        _ => "broda".to_string(),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (jboshow method)
pub fn jboshow_prop(prop: &JboProp) -> String {
    jboshow_prop_with_lambdas(prop, &[])
}

// Ported from: JboShow.hs :: logjboshow' Prop (modal prefix rendering for logical output)
fn logshow_modal_prefix(modal_op: &JboModalOp) -> Vec<String> {
    match modal_op {
        JboModalOp::Tagged(tag, mt) => {
            let mut parts = vec![format!("({})", jboshow_tag(tag)), "(".to_string()];
            if let Some(t) = mt {
                parts.push(logshow_term(t));
            }
            parts.push("). ".to_string());
            parts
        }
        JboModalOp::WithEventAs(t) => vec![logshow_term(t), "=. ".to_string()],
        JboModalOp::QTruthModal => vec!["?. ".to_string()],
        JboModalOp::NonVeridical => vec!["non-veridical: ".to_string()],
    }
}

// Ported from: JboParse.hs :: parseTag + JboProp.hs :: connToFol
// Expands connected logical tags into FOL connectives
fn expand_connected_tag_modal(
    tag: &crate::jbo_prop::JboTag,
    mt: &Option<JboTerm>,
    inner: &JboProp,
) -> Option<JboProp> {
    if let crate::jbo_prop::JboTag::ConnectedTag(con, tag1, tag2) = tag {
        if let crate::jbo_prop::JboConnective::JboConnLog(_, lcon) = con.as_ref() {
            let left = Prop::Modal(
                JboModalOp::Tagged((**tag1).clone(), mt.clone()),
                Box::new(inner.clone()),
            );
            let right = Prop::Modal(
                JboModalOp::Tagged((**tag2).clone(), mt.clone()),
                Box::new(inner.clone()),
            );
            Some(crate::jbo_prop::conn_to_fol(lcon, left, right))
        } else {
            None
        }
    } else {
        None
    }
}

// Ported from: JboShow.hs :: logjboshow' Prop (prefix accumulation for logical output)
fn logshow_prop_with_prefixes(prop: &JboProp, mut prefixes: Vec<String>) -> String {
    match prop {
        Prop::Modal(modal_op, inner) => {
            if let JboModalOp::Tagged(tag, mt) = modal_op {
                if let Some(expanded) = expand_connected_tag_modal(tag, mt, inner) {
                    return logshow_prop_with_prefixes(&expanded, prefixes);
                }
            }
            if matches!(modal_op, JboModalOp::NonVeridical) {
                let mut parts = logshow_modal_prefix(modal_op);
                parts.push(logshow_prop_with_prefixes(inner, prefixes));
                parts.concat()
            } else {
                prefixes.extend(logshow_modal_prefix(modal_op));
                logshow_prop_with_prefixes(inner, prefixes)
            }
        }
        _ if prefixes.is_empty() => logshow_prop_without_prefixes(prop),
        _ => {
            let mut parts = prefixes;
            parts.push(logshow_prop_without_prefixes(prop));
            parts.concat()
        }
    }
}

// Ported from: JboShow.hs :: logjboshow' Prop (main proposition formatting without prefixes)
fn logshow_prop_without_prefixes(prop: &JboProp) -> String {
    match prop {
        Prop::Rel(rel, terms) => {
            let rel_str = logshow_rel(rel);
            let term_strs: Vec<String> = terms.iter().map(logshow_term).collect();
            if term_strs.is_empty() {
                format!("{}()", rel_str)
            } else {
                format!("{}({})", rel_str, term_strs.join(","))
            }
        }
        Prop::Connected(con, p1, p2) => {
            let left = logshow_prop(p1);
            let right = logshow_prop(p2);
            match con {
                crate::logic::Connective::And => format!("({} /\\ {})", left, right),
                crate::logic::Connective::Or => format!("({} \\/ {})", left, right),
                crate::logic::Connective::Impl => format!("({} => {})", left, right),
                crate::logic::Connective::Equiv => format!("({} <=> {})", left, right),
            }
        }
        Prop::Not(p) => format!("~{}", logshow_prop(p)),
        Prop::Modal(_, _) => logshow_prop_with_prefixes(prop, Vec::new()),
        _ => "Prop(...)".to_string(),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (logshow method)
pub fn logshow_prop(prop: &JboProp) -> String {
    logshow_prop_with_prefixes(prop, Vec::new())
}

// Ported from: JboShow.hs :: instance JboShow JboProp (jboshow method)
/// Convert proposition to Lojban text
pub fn prop_to_lojban(prop: &JboProp) -> String {
    jboshow_prop(prop)
}

// Ported from: JboShow.hs :: instance JboShow JboFragment (jboshow method for JboFragTerms)
/// Build Lojban output for fragment with terms
/// Fragments show ju'a nai prefixes for non-veridical propositions,
/// followed by .i and the fragment terms.
pub fn build_lojban_output_from_fragment(
    terms: &[JboTerm],
    side_props: &[JboProp],
) -> String {
    let mut output_parts = vec![];

    for prop in side_props {
        let loj = jboshow_prop(prop);
        for line in loj.lines().filter(|line| !line.is_empty()) {
            if output_parts.is_empty() || line.starts_with(".i ") {
                output_parts.push(line.to_string());
            } else {
                output_parts.push(format!(".i {}", line));
            }
        }
    }

    if !terms.is_empty() {
        let term_strs: Vec<String> = terms.iter().map(jboshow_term).collect();
        let frag = term_strs.join(" ");
        if output_parts.is_empty() || frag.starts_with(".i ") {
            output_parts.push(frag);
        } else {
            output_parts.push(format!(".i {}", frag));
        }
    } else if !output_parts.is_empty() {
        output_parts.push(".i ".to_string());
    }

    output_parts.join("\n")
}
