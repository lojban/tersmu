//! CLI eval/show bridge for semantic analysis from `jbo_parse`.
//!
//! Ports the Haskell path `ParseText.hs` → `JboParse.hs :: evalText` → `JboShow.hs` by formatting
//! `SemanticResult` values into the logical and canonical output strings consumed by the Rust CLI.
//! Display helpers mirror the `JboShow`/`LogShow` instances while using explicit Rust formatter
//! context instead of `Bindful.hs` state.

use crate::jbo_parse::{eval_text, eval_text_with_options, SemanticResult};
use crate::jbo_prop::{
    JboFragment, JboMex, JboModalOp, JboOperator, JboProp, JboQuantifier, JboRel, JboTerm,
    Texticule,
};
use crate::jbo_syntax::{Lerfu, SumtiAtom, Text};
use crate::logic::Prop;

// Rust-only: Inverse of jbonum for parsing PA digits back to numeric strings
fn format_pa_digit(pa: &str) -> &str {
    match pa {
        "no" => "0",
        "pa" => "1",
        "re" => "2",
        "ci" => "3",
        "vo" => "4",
        "mu" => "5",
        "xa" => "6",
        "ze" => "7",
        "bi" => "8",
        "so" => "9",
        _ => pa,
    }
}

// Ported from: JboShow.hs :: instance JboShow LerfuString (logshow method)
fn format_lerfu_string(ls: &[Lerfu]) -> String {
    ls.iter().map(format_lerfu).collect::<String>()
}

// Ported from: JboShow.hs :: instance JboShow Lerfu (logshow method)
fn format_lerfu(l: &Lerfu) -> String {
    match l {
        Lerfu::LerfuChar(c) => c.to_string(),
        Lerfu::LerfuPA(p) | Lerfu::LerfuValsi(p) | Lerfu::LerfuShift(p) => format!("{{{}}}", p),
        Lerfu::LerfuShifted(lau, l) => format!("{{{}}}({})", lau, format_lerfu(l)),
        Lerfu::LerfuComposite(ls) => format!(
            "[{}]",
            ls.iter().map(format_lerfu).collect::<Vec<_>>().join(",")
        ),
    }
}

// Rust-only: Debug helper for Lerfu, not present in Haskell
fn format_lerfu_debug(l: &Lerfu) -> String {
    match l {
        Lerfu::LerfuChar(c) => format!("LerfuChar '{}'", c),
        Lerfu::LerfuPA(p) => format!("LerfuPA {:?}", p),
        Lerfu::LerfuValsi(v) => format!("LerfuValsi {:?}", v),
        Lerfu::LerfuShift(s) => format!("LerfuShift {:?}", s),
        Lerfu::LerfuShifted(lau, l) => {
            format!("LerfuShifted {:?} ({})", lau, format_lerfu_debug(l))
        }
        Lerfu::LerfuComposite(ls) => format!(
            "LerfuComposite [{}]",
            ls.iter()
                .map(format_lerfu_debug)
                .collect::<Vec<_>>()
                .join(",")
        ),
    }
}

// Ported from: JboShow.hs :: instance JboShow Lerfu (jboshow method)
fn format_lerfu_jbo(l: &Lerfu) -> String {
    match l {
        Lerfu::LerfuChar(c) if matches!(c, 'a' | 'e' | 'i' | 'o' | 'u') => format!("{}bu", c),
        Lerfu::LerfuChar('y') => "y bu".to_string(),
        Lerfu::LerfuChar('h') => "y'y".to_string(),
        Lerfu::LerfuChar(c) if c.is_ascii_digit() => jbonum(c.to_digit(10).unwrap() as i32),
        Lerfu::LerfuChar(c) => format!("{}y", c),
        Lerfu::LerfuPA(p) => p.clone(),
        Lerfu::LerfuValsi(v) => format!("{} bu", v),
        Lerfu::LerfuShift(s) => s.clone(),
        Lerfu::LerfuShifted(lau, l) => format!("{} {}", lau, format_lerfu_jbo(l)),
        Lerfu::LerfuComposite(ls) => format!(
            "tei {} foi",
            ls.iter()
                .map(format_lerfu_jbo)
                .collect::<Vec<_>>()
                .join(" ")
        ),
    }
}

// Ported from: JboShow.hs :: vowelnum
fn vowelnum(n: i32) -> String {
    match n {
        1 => "a".to_string(),
        2 => "e".to_string(),
        3 => "i".to_string(),
        4 => "o".to_string(),
        5 => "u".to_string(),
        _ => n.to_string(),
    }
}

// Wrapper for jbo_show::jbonum
fn jbonum(n: i32) -> String {
    crate::jbo_show::jbonum(n)
}

// Ported from: JboShow.hs :: instance JboShow ShowBindable (jboshow method)
// Covers SAss (ko'a/ko'e/..., fo'a/fo'e/...) and MainBridiSumbasti (vo'a/vo'e/...)
fn format_sumti_atom(atom: &SumtiAtom) -> String {
    let jbo = match atom {
        SumtiAtom::Assignable(n) if *n <= 5 => format!("ko'{}", vowelnum(*n)),
        SumtiAtom::Assignable(n) if *n <= 10 => format!("fo'{}", vowelnum(*n - 5)),
        SumtiAtom::Assignable(n) => format!("ko'a xi {}", jbonum(*n)),
        SumtiAtom::Ri(0 | 1) => "ri".to_string(),
        SumtiAtom::Ri(n) => format!("ri xi {}", jbonum(*n)),
        SumtiAtom::Ra(r) => r.clone(),
        SumtiAtom::MainBridiSumbasti(n) if *n <= 5 => format!("vo'{}", vowelnum(*n)),
        SumtiAtom::MainBridiSumbasti(n) => format!("vo'a xi {}", jbonum(*n)),
        SumtiAtom::LerfuString(ls) => return format_lerfu_string(ls),
        _ => format!("{:?}", atom),
    };
    format!("{{{}}}", jbo)
}

// Rust-only: Debug helper for syntax-level Operator, not present in Haskell
fn format_syntax_operator(op: &crate::jbo_syntax::Operator) -> String {
    match op {
        crate::jbo_syntax::Operator::OpVUhU(s) => format!("OpVUhU \"{}\"", s),
        _ => format!("{:?}", op),
    }
}

// Rust-only: Debug helper for syntax-level Mex, not present in Haskell
fn format_syntax_mex(mex: &crate::jbo_syntax::Mex) -> String {
    match mex {
        crate::jbo_syntax::Mex::Operation(op, args) => {
            let args = args
                .iter()
                .map(format_syntax_mex)
                .collect::<Vec<_>>()
                .join(",");
            format!("Operation ({}) [{}]", format_syntax_operator(op), args)
        }
        crate::jbo_syntax::Mex::MexNumeralString(ns) => {
            let parts = ns
                .iter()
                .map(|n| match n {
                    crate::jbo_syntax::Numeral::PA(s) => s.clone(),
                    crate::jbo_syntax::Numeral::NumeralLerfu(l) => format!("{:?}", l),
                })
                .collect::<Vec<_>>();
            let digits = parts.join("");
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
                _ => parts.join(" "),
            }
        }
        crate::jbo_syntax::Mex::MexInt(n) => format!("MexInt {}", n),
        crate::jbo_syntax::Mex::MexLerfuString(ls) => format!(
            "MexLerfuString [{}]",
            ls.iter()
                .map(format_lerfu_debug)
                .collect::<Vec<_>>()
                .join(",")
        ),
        _ => format!("{:?}", mex),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboMex (logshow method)
fn format_mex(mex: &JboMex) -> String {
    match mex {
        JboMex::MexNumeralString(ns) => format!(
            "({})",
            ns.iter()
                .map(|n| match n {
                    crate::jbo_syntax::Numeral::PA(s) => s.clone(),
                    crate::jbo_syntax::Numeral::NumeralLerfu(l) => format_lerfu(l),
                })
                .collect::<Vec<_>>()
                .join(" ")
        ),
        JboMex::MexInt(n) => n.to_string(),
        JboMex::MexLerfuString(ls) => format!(
            "({})",
            ls.iter()
                .map(format_lerfu_jbo)
                .collect::<Vec<_>>()
                .join(" ")
        ),
        JboMex::Operation(op, args) => {
            let args = args.iter().map(format_mex).collect::<Vec<_>>().join(",");
            format!("{}({})", format_operator(op), args)
        }
        JboMex::ConnectedMex(_, con, m1, m2) => format!(
            "({}){}({})",
            format_mex(m1),
            format_connective(".", con),
            format_mex(m2)
        ),
        JboMex::QualifiedMex(qual, m) => {
            let qualifier = match qual {
                crate::jbo_syntax::SumtiQualifier::LAhE(l) => l.clone(),
                crate::jbo_syntax::SumtiQualifier::NAhE_BO(n) => format!("{} bo", n),
            };
            format!("{{{}}}({})", qualifier, format_mex(m))
        }
        JboMex::MexSelbri(s) => format!("[{}]", format_seltau_prop(&s(&[JboTerm::BoundVar(0)]))),
        JboMex::MexArray(ms) => format!(
            "[{}]",
            ms.iter().map(format_mex).collect::<Vec<_>>().join(",")
        ),
        JboMex::MexSumti(t) => format!("[{}]", format_term(t)),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboMex (logshow method for numeric cases)
fn format_mex_number_for_rel(mex: &JboMex) -> String {
    match mex {
        JboMex::MexNumeralString(ns) => format!(
            "({})",
            ns.iter()
                .map(|n| match n {
                    crate::jbo_syntax::Numeral::PA(s) => s.clone(),
                    crate::jbo_syntax::Numeral::NumeralLerfu(l) => format_lerfu(l),
                })
                .collect::<Vec<_>>()
                .join(" ")
        ),
        JboMex::MexLerfuString(ls) => format!(
            "({})",
            ls.iter()
                .map(format_lerfu_jbo)
                .collect::<Vec<_>>()
                .join(" ")
        ),
        JboMex::MexInt(n) => n.to_string(),
        _ => format_mex(mex),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboQuantifier (logshow method)
fn format_quantifier(q: &JboQuantifier) -> String {
    match q {
        JboQuantifier::QuestionQuantifier => "?".to_string(),
        JboQuantifier::LojQuantifier(q) => q.to_string(),
        JboQuantifier::MexQuantifier(m) => format_mex(m),
        JboQuantifier::RelQuantifier(q) => format!("RelQuantifier({})", format_quantifier(q)),
    }
}

// Rust-only: Helper for formatting quantified variable prefixes in logical output
fn format_quantified_prefix(q: &JboQuantifier, name: &str) -> String {
    match q {
        JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists)
        | JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Forall) => {
            format!("{}{}", format_quantifier(q), name)
        }
        _ => format!("{} {}", format_quantifier(q), name),
    }
}

// Ported from: JboShow.hs :: instance JboShow Texticule (logshow method)
fn format_texticule(texticule: &Texticule) -> String {
    match texticule {
        Texticule::TexticuleFrag(fragment) => match fragment {
            JboFragment::JboFragTerms(terms) => format!(
                "[Fragment: {}]",
                terms.iter().map(format_term).collect::<Vec<_>>().join(",")
            ),
            JboFragment::JboFragUnparsed(_) => "[Fragment]".to_string(),
        },
        Texticule::TexticuleProp(prop) => format_prop(prop),
        Texticule::TexticuleSide(_, inner) => format_texticule(inner),
    }
}

// Ported from: JboShow.hs :: instance JboShow [Texticule] (logshow method)
// Includes term-attached side texticules from JboParse.hs
fn format_text(text: &[Texticule]) -> String {
    let mut lines = text.iter().map(format_texticule).collect::<Vec<_>>();
    let mut side_lines = Vec::new();
    for texticule in text {
        collect_term_side_texticules_from_texticule(texticule, &mut side_lines);
    }
    let side_lines = side_lines
        .into_iter()
        .map(|texticule| format_texticule(&texticule))
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>();
    if !side_lines.is_empty() {
        let insert_at = lines
            .iter()
            .take_while(|line| line.starts_with("non-veridical: "))
            .count();
        for (i, line) in side_lines.into_iter().enumerate() {
            lines.insert(insert_at + i, line);
        }
    }
    lines.join("\n")
}

// Ported from: JboShow.hs :: instance JboShow JboTerm (logshow method)
fn format_term(term: &JboTerm) -> String {
    match term {
        JboTerm::NonAnaph(s) => s.clone(),
        JboTerm::Named(s) => format!("\"{}\"", s),
        JboTerm::PredNamed(pred) => {
            let prop = pred(&JboTerm::Named("_".to_string()));
            format!("[Name: {} ]", format_prop(&prop).replace("\"_\"", "_"))
        }
        JboTerm::Constant(n, args) if args.is_empty() => format!("c{}", n),
        JboTerm::Constant(n, args) => {
            let arg_strs = args.iter().map(format_term).collect::<Vec<_>>().join(",");
            format!("f{}({})", n, arg_strs)
        }
        JboTerm::Var(_) => "[UNBOUND Var]".to_string(),
        JboTerm::BoundVar(n) if *n < 0 => format!("\\{}", n.abs()),
        JboTerm::BoundVar(n) => format!("x{}", n),
        JboTerm::Unfilled => " ".to_string(),
        JboTerm::UnboundSumbasti(atom) => format_sumti_atom(atom),
        JboTerm::Value(m) => format_mex(m),
        JboTerm::QualifiedTerm(qual, t) => {
            let qualifier = match qual {
                crate::jbo_syntax::SumtiQualifier::LAhE(l) => l.clone(),
                crate::jbo_syntax::SumtiQualifier::NAhE_BO(n) => format!("{} bo", n),
            };
            format!("{{{}}}({})", qualifier, format_term(t))
        }
        JboTerm::TheMex(m) => format!("[MEX: {}]", format_syntax_mex(m)),
        JboTerm::JoikedTerms(joik, t1, t2) => {
            format!("({} {{{}}} {})", format_term(t1), joik, format_term(t2))
        }
        JboTerm::JboQuote(text) => format!("«{}»", format_text(text)),
        JboTerm::JboErrorQuote(words) => format!("<{{< {} >}}>", words.join(" ")),
        JboTerm::JboNonJboQuote(s) => format!("<[< {} >]>", s),
        JboTerm::Valsi(s) => format!("{{{}}}", s),
        JboTerm::TermWithSides(t, _) => format_term(t),
    }
}

// Ported from: JboShow.hs :: instance JboShow Connective (logshow method for LogJboConnective)
fn format_log_connective(prefix: &str, con: &crate::jbo_syntax::LogJboConnective) -> String {
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

// Ported from: JboShow.hs :: instance JboShow Connective (logshow method)
fn format_connective(prefix: &str, con: &crate::jbo_syntax::Connective) -> String {
    let (mut lojban, mtag) = match con {
        crate::jbo_syntax::Connective::JboConnLog(mtag, lcon) => {
            (format_log_connective(prefix, lcon), mtag.as_deref())
        }
        crate::jbo_syntax::Connective::JboConnJoik(mtag, joik) => {
            let base = if joik == "??" {
                if prefix == "." {
                    "ji".to_string()
                } else {
                    "je'i".to_string()
                }
            } else {
                joik.clone()
            };
            (base, mtag.as_deref())
        }
    };
    if let Some(tag) = mtag {
        lojban.push(' ');
        lojban.push_str(&format_tag_syntax(tag));
        lojban.push_str(" bo");
    }
    format!("{{{}}}", lojban)
}

// Ported from: JboShow.hs :: instance JboShow Tag (jboshow method for syntax-level Tag)
fn format_tag_syntax(tag: &crate::jbo_syntax::Tag) -> String {
    match tag {
        crate::jbo_syntax::Tag::DecoratedTagUnits(dtus) => dtus
            .iter()
            .map(|dtu| match &dtu.tag_unit {
                crate::jbo_syntax::TagUnit::BAI(s)
                | crate::jbo_syntax::TagUnit::TenseCmavo(s)
                | crate::jbo_syntax::TagUnit::CAhA(s)
                | crate::jbo_syntax::TagUnit::CUhE(s) => s.clone(),
                crate::jbo_syntax::TagUnit::FAhA { faha_cmavo, .. } => faha_cmavo.clone(),
                crate::jbo_syntax::TagUnit::TAhE_ZAhO {
                    tahe_zaho_cmavo, ..
                } => tahe_zaho_cmavo.clone(),
                crate::jbo_syntax::TagUnit::ROI {
                    roiroi,
                    roi_quantifier,
                    ..
                } => format!("{} {}", format_syntax_mex(roi_quantifier), roiroi),
                crate::jbo_syntax::TagUnit::FIhO(_) => "fi'o".to_string(),
                crate::jbo_syntax::TagUnit::KI => "ki".to_string(),
            })
            .collect::<Vec<_>>()
            .join(" "),
        crate::jbo_syntax::Tag::ConnectedTag(con, tag1, tag2) => format!(
            "{} {} {}",
            format_tag_syntax(tag1),
            format_connective("j", con),
            format_tag_syntax(tag2)
        ),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboOperator (logshow method)
fn format_operator(op: &JboOperator) -> String {
    match op {
        JboOperator::ConnectedOperator(_, con, o1, o2) => format!(
            "<{}>{}<{}>",
            format_operator(o1),
            format_connective(".", con),
            format_operator(o2)
        ),
        JboOperator::OpPermuted(s, op) => {
            format!("{} {}", crate::jbo_show::se_to_str(*s), format_operator(op))
        }
        JboOperator::OpScalarNegated(n, op) => format!("{{{}}}({})", n, format_operator(op)),
        JboOperator::OpMex(m) => format!("[{}]", format_mex(m)),
        JboOperator::OpSelbri(s) => {
            format!("[{}]", format_seltau_prop(&s(&[JboTerm::BoundVar(0)])))
        }
        JboOperator::OpVUhU(s) => format!("{{{}}}", s),
    }
}

// Ported from: JboShow.hs :: instance JboShow Connective (logshow method for proposition connectives)
fn format_prop_log_connective(prefix: &str, con: &crate::jbo_syntax::LogJboConnective) -> String {
    format!(
        "{}{}{}",
        if con.b1 { "" } else { "na " },
        if con.c == 'U' {
            format!("se {}u", prefix)
        } else {
            format!("{}{}", prefix, con.c)
        },
        if con.b2 { "" } else { " nai" }
    )
}

// Ported from: JboShow.hs :: instance JboShow JboConnective (logshow method for semantic connectives)
fn format_prop_connective(
    prefix: &str,
    con: &crate::jbo_prop::JboConnective,
    logical: bool,
) -> String {
    let shown = match con {
        crate::jbo_prop::JboConnective::JboConnLog(mtag, lcon) => {
            let mut s = format_prop_log_connective(prefix, lcon);
            if let Some(tag) = mtag {
                s.push(' ');
                s.push_str(&format_tag_with_mode(tag, false));
                s.push_str(" bo");
            }
            s
        }
        crate::jbo_prop::JboConnective::JboConnJoik(mtag, joik) => {
            let mut s = if joik == "??" {
                match prefix {
                    "." => "ji".to_string(),
                    "j" => "je'i".to_string(),
                    _ => "BUG".to_string(),
                }
            } else {
                joik.clone()
            };
            if let Some(tag) = mtag {
                s.push(' ');
                s.push_str(&format_tag_with_mode(tag, false));
                s.push_str(" bo");
            }
            s
        }
    };
    if logical {
        format!("{{{}}}", shown)
    } else {
        shown
    }
}

// Ported from: JboShow.hs :: instance JboShow JboTag (logshow/jboshow method with mode parameter)
fn format_tag_with_mode(tag: &crate::jbo_prop::JboTag, logical: bool) -> String {
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
                    crate::jbo_prop::JboTagUnit::ROI(s, true, q) => {
                        format!("fe'e {} {}", format_mex(q), s)
                    }
                    crate::jbo_prop::JboTagUnit::ROI(s, _, q) => format!("{} {}", format_mex(q), s),
                    crate::jbo_prop::JboTagUnit::FIhO(p) => format!(
                        "fi'o {}",
                        format_seltau_prop(&p(&[crate::jbo_prop::JboTerm::BoundVar(0)]))
                    ),
                    crate::jbo_prop::JboTagUnit::KI => "ki".to_string(),
                };
                format!(
                    "{}{}{}{}",
                    dtu.nahe
                        .as_ref()
                        .map(|s| format!("{} ", s))
                        .unwrap_or_default(),
                    dtu.se
                        .map(|se| format!("{} ", crate::jbo_show::se_to_str(se)))
                        .unwrap_or_default(),
                    unit,
                    if dtu.nai { " nai" } else { "" }
                )
            })
            .collect::<Vec<_>>()
            .join(" "),
        crate::jbo_prop::JboTag::ConnectedTag(con, tag1, tag2) => {
            let connective = format_prop_connective("j", con, logical);
            if logical {
                format!(
                    "{}({},{})",
                    connective,
                    format_tag_with_mode(tag1, false),
                    format_tag_with_mode(tag2, false)
                )
            } else {
                format!(
                    "{} {} {}",
                    format_tag_with_mode(tag1, false),
                    connective,
                    format_tag_with_mode(tag2, false)
                )
            }
        }
    }
}

// Ported from: JboShow.hs :: instance JboShow JboTag (logshow method)
fn format_tag(tag: &crate::jbo_prop::JboTag) -> String {
    format_tag_with_mode(tag, true)
}

// Ported from: JboShow.hs :: instance JboShow JboModalOp (logshow method for modal relations)
fn format_modal_rel(modal: &JboModalOp, rel: &JboRel) -> String {
    match modal {
        JboModalOp::Tagged(tag, term) => {
            let term = term.as_ref().map(format_term).unwrap_or_default();
            format!("({})({}). {}(_)", format_tag(tag), term, format_rel(rel))
        }
        _ => format!("Modal({:?}). {}(_)", modal, format_rel(rel)),
    }
}

// Ported from: JboShow.hs :: logjboshow' Prop (quantifier restriction term rendering)
fn format_restriction_term(term: &JboTerm, var: i32) -> String {
    match term {
        JboTerm::TermWithSides(inner, _) => format_restriction_term(inner, var),
        JboTerm::BoundVar(n) if *n == var => "_".to_string(),
        JboTerm::Constant(n, args) if !args.is_empty() => {
            let arg_strs: Vec<String> = args
                .iter()
                .map(|t| format_restriction_term(t, var))
                .collect();
            format!("f{}({})", n, arg_strs.join(","))
        }
        JboTerm::QualifiedTerm(qual, t) => {
            let qualifier = match qual {
                crate::jbo_syntax::SumtiQualifier::LAhE(l) => l.clone(),
                crate::jbo_syntax::SumtiQualifier::NAhE_BO(n) => format!("{} bo", n),
            };
            format!("{{{}}}({})", qualifier, format_restriction_term(t, var))
        }
        JboTerm::JoikedTerms(joik, t1, t2) => format!(
            "({} {{{}}} {})",
            format_restriction_term(t1, var),
            joik,
            format_restriction_term(t2, var)
        ),
        _ => format_term(term),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboTerm (logshow method for lambda terms)
fn format_lambda_term(term: &JboTerm) -> String {
    match term {
        JboTerm::BoundVar(n) if *n < 0 => format!("\\{}", n.abs()),
        _ => format_term(term),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboRel (logshow method for lambda relations)
fn format_lambda_rel(rel: &JboRel) -> String {
    match rel {
        JboRel::AppliedRel(r, terms) => {
            format!(
                "{}({})",
                format_rel(r),
                terms
                    .iter()
                    .map(format_lambda_term)
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
        _ => format_rel(rel),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboTerm (logshow method for lambda restriction terms)
fn format_lambda_restriction_term(term: &JboTerm, var: i32) -> String {
    match term {
        JboTerm::BoundVar(n) if *n == var => "_".to_string(),
        _ => format_lambda_term(term),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (logshow method for lambda restriction propositions)
fn format_lambda_restriction_prop(
    prop: &Prop<
        JboRel,
        JboTerm,
        String,
        crate::jbo_prop::JboModalOp,
        crate::jbo_prop::JboQuantifier,
    >,
    var: i32,
) -> String {
    match prop {
        Prop::Rel(JboRel::Among(t), terms) if matches!(terms.as_slice(), [JboTerm::BoundVar(n)] if *n == var) =>
        {
            format!("(_ ≤ {})", format_lambda_restriction_term(t, var))
        }
        Prop::Rel(JboRel::AppliedRel(r, applied_terms), terms)
            if terms.is_empty()
                && matches!(r.as_ref(), JboRel::Among(_))
                && matches!(applied_terms.as_slice(), [JboTerm::BoundVar(n)] if *n == var) =>
        {
            if let JboRel::Among(t) = r.as_ref() {
                format!("(_ ≤ {})", format_lambda_restriction_term(t, var))
            } else {
                unreachable!()
            }
        }
        Prop::Rel(rel, terms) => {
            format!(
                "{}({})",
                format_lambda_rel(rel),
                terms
                    .iter()
                    .map(|t| format_lambda_restriction_term(t, var))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
        _ => format_lambda_prop(prop),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboRel (logshow method for restriction relations)
fn format_restriction_rel(rel: &JboRel, var: i32) -> String {
    match rel {
        JboRel::AppliedRel(r, terms) => format!(
            "{}({})",
            format_rel(r),
            terms
                .iter()
                .map(|t| format_restriction_term(t, var))
                .collect::<Vec<_>>()
                .join(",")
        ),
        _ => format_rel(rel),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (logshow method for restriction propositions)
fn format_restriction_prop(
    prop: &Prop<
        JboRel,
        JboTerm,
        String,
        crate::jbo_prop::JboModalOp,
        crate::jbo_prop::JboQuantifier,
    >,
    var: i32,
) -> String {
    match prop {
        Prop::Rel(rel, terms) => {
            if matches!(rel, JboRel::Equal) {
                return format!(
                    "({})",
                    terms
                        .iter()
                        .map(|t| format_restriction_term(t, var))
                        .collect::<Vec<_>>()
                        .join(" == ")
                );
            }
            if let JboRel::Among(t) = rel {
                if matches!(terms.as_slice(), [JboTerm::BoundVar(n)] if *n == var) {
                    return format!("(_ ≤ {})", format_restriction_term(t, var));
                }
                let terms_str = terms
                    .iter()
                    .map(|term| format_restriction_term(term, var))
                    .collect::<Vec<_>>()
                    .join(",");
                return format!("({} ≤ {})", terms_str, format_restriction_term(t, var));
            }
            if let JboRel::AppliedRel(r, applied_terms) = rel {
                if let JboRel::Among(t) = r.as_ref() {
                    if terms.is_empty()
                        && matches!(applied_terms.as_slice(), [JboTerm::BoundVar(n)] if *n == var)
                    {
                        return format!("(_ ≤ {})", format_restriction_term(t, var));
                    }
                }
            }
            format!(
                "{}({})",
                format_restriction_rel(rel, var),
                terms
                    .iter()
                    .map(|t| format_restriction_term(t, var))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::NonVeridical, inner) => {
            format!("non-veridical: {}", format_restriction_prop(inner, var))
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::Tagged(tag, mt), inner) => {
            let term = mt
                .as_ref()
                .map(|t| format_restriction_term(t, var))
                .unwrap_or_default();
            format!(
                "({})({}). {}",
                format_tag(tag),
                term,
                format_restriction_prop(inner, var)
            )
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::WithEventAs(t), inner) => {
            format!(
                "{}=. {}",
                format_restriction_term(t, var),
                format_restriction_prop(inner, var)
            )
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::QTruthModal, inner) => {
            format!("?. {}", format_restriction_prop(inner, var))
        }
        Prop::Not(inner) => format!("¬{}", format_restriction_prop(inner, var)),
        Prop::Connected(conn, p1, p2) => {
            format!(
                "({} {} {})",
                format_restriction_prop(p1, var),
                conn,
                format_restriction_prop(p2, var)
            )
        }
        Prop::NonLogConnected(joik, p1, p2) => {
            format!(
                "({} {{{}}} {})",
                format_restriction_prop(p1, var),
                joik,
                format_restriction_prop(p2, var)
            )
        }
        Prop::Quantified(q, restriction, body) => {
            let n = var + 1;
            let mut prefix = match q {
                JboQuantifier::RelQuantifier(inner_q) => {
                    format_quantified_prefix(inner_q, &format!("R{}", n))
                }
                _ => format_quantified_prefix(q, &format!("x{}", n)),
            };
            if let Some(r) = restriction {
                prefix.push_str(":(");
                prefix.push_str(&format_restriction_prop(&r(n), n));
                prefix.push_str("). ");
            } else {
                prefix.push_str(". ");
            }
            format!("{}{}", prefix, format_restriction_prop(&body(n), var))
        }
        Prop::Eet => "_|_".to_string(),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (logshow method for lambda propositions)
fn format_lambda_prop(
    prop: &Prop<
        JboRel,
        JboTerm,
        String,
        crate::jbo_prop::JboModalOp,
        crate::jbo_prop::JboQuantifier,
    >,
) -> String {
    format_lambda_prop_at(prop, 1)
}

// Ported from: JboShow.hs :: instance JboShow JboProp (logshow method for lambda propositions with variable tracking)
fn format_lambda_prop_at(
    prop: &Prop<
        JboRel,
        JboTerm,
        String,
        crate::jbo_prop::JboModalOp,
        crate::jbo_prop::JboQuantifier,
    >,
    next_var: i32,
) -> String {
    match prop {
        Prop::Rel(rel, terms) => {
            format!(
                "{}({})",
                format_lambda_rel(rel),
                terms
                    .iter()
                    .map(format_lambda_term)
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::Tagged(tag, mt), inner) => {
            if let Some(expanded) = expand_connected_tag_modal(tag, mt, inner) {
                return format_lambda_prop_at(&expanded, next_var);
            }
            let term = mt.as_ref().map(format_lambda_term).unwrap_or_default();
            format!(
                "({})({}). {}",
                format_tag(tag),
                term,
                format_lambda_prop_at(inner, next_var)
            )
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::WithEventAs(term), inner) => {
            format!(
                "{}=. {}",
                format_lambda_term(term),
                format_lambda_prop_at(inner, next_var)
            )
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::QTruthModal, inner) => {
            format!("?. {}", format_lambda_prop_at(inner, next_var))
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::NonVeridical, inner) => {
            format!("non-veridical: {}", format_lambda_prop_at(inner, next_var))
        }
        Prop::Not(inner) => format!("¬{}", format_lambda_prop_at(inner, next_var)),
        Prop::Connected(conn, p1, p2) => {
            format!(
                "({} {} {})",
                format_lambda_prop_at(p1, next_var),
                conn,
                format_lambda_prop_at(p2, next_var)
            )
        }
        Prop::NonLogConnected(joik, p1, p2) => {
            format!(
                "({} {{{}}} {})",
                format_lambda_prop_at(p1, next_var),
                joik,
                format_lambda_prop_at(p2, next_var)
            )
        }
        Prop::Quantified(q, restriction, body) => {
            let n = next_var;
            let mut prefix = match q {
                JboQuantifier::RelQuantifier(inner_q) => {
                    format_quantified_prefix(inner_q, &format!("R{}", n))
                }
                _ => format_quantified_prefix(q, &format!("x{}", n)),
            };
            if let Some(r) = restriction {
                prefix.push_str(":(");
                prefix.push_str(&format_lambda_restriction_prop(&r(n), n));
                prefix.push_str("). ");
            } else {
                prefix.push_str(". ");
            }
            let body_next_var = if matches!(q, JboQuantifier::RelQuantifier(_)) {
                n
            } else {
                n + 1
            };
            format!(
                "{}{}",
                prefix,
                format_lambda_prop_at(&body(n), body_next_var)
            )
        }
        Prop::Eet => "_|_".to_string(),
    }
}

// Ported from: JboShow.hs :: logjboshow' Rel (scalar negation case)
// Mirrors scalar NAhE display after parsedSelbriToNewSelbri creates applied rel with skipped x2
fn format_scalar_negated_rel_inner(rel: &JboRel) -> String {
    match rel {
        JboRel::AppliedRel(r, terms)
            if terms.len() > 2 && matches!(terms.get(1), Some(JboTerm::Unfilled)) =>
        {
            let terms = std::iter::once(terms[0].clone())
                .chain(terms.iter().skip(2).cloned())
                .collect::<Vec<_>>();
            format!(
                "{}({})",
                format_rel(r),
                terms.iter().map(format_term).collect::<Vec<_>>().join(",")
            )
        }
        _ => format_rel(rel),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboRel (logshow method)
fn format_rel(rel: &JboRel) -> String {
    match rel {
        JboRel::Brivla(s) => s.clone(),
        JboRel::Equal => "==".to_string(),
        JboRel::Among(t) => format!("( ≤ {})", format_term(t)),
        JboRel::Tanru(r1, r2) => {
            let seltau_str = format_seltau(r1);
            let tertau_str = format_rel(r2);
            format!("<{}><{}>", seltau_str, tertau_str)
        }
        JboRel::AppliedRel(r, terms) => {
            format!(
                "{}({})",
                format_rel(r),
                terms.iter().map(format_term).collect::<Vec<_>>().join(",")
            )
        }
        JboRel::TanruConnective(con, r1, r2) => {
            format!(
                "<{}>{}<{}>",
                format_seltau(r1),
                format_connective("j", con),
                format_seltau(r2)
            )
        }
        JboRel::PermutedRel(n, r) => {
            format!("{} {}", crate::jbo_show::se_to_str(*n), format_rel(r))
        }
        JboRel::RVar(_) => "[UNBOUND RVar]".to_string(),
        JboRel::BoundRVar(n) => format!("R{}", n),
        JboRel::RAss(n) => format!("R{}", n),
        JboRel::UnboundBribasti(tu) => match tu {
            crate::jbo_syntax::TanruUnit::TUGOhA(g, n) => {
                if *n == 1 {
                    g.clone()
                } else {
                    format!("{}{}", g, n)
                }
            }
            crate::jbo_syntax::TanruUnit::TUBrivla(s) => s.clone(),
            _ => format!("unbound({:?})", tu),
        },
        JboRel::Moi(t, moi) => match t.as_ref() {
            JboTerm::Value(
                m @ (JboMex::MexInt(_) | JboMex::MexNumeralString(_) | JboMex::MexLerfuString(_)),
            ) => format!("{} {}", format_mex_number_for_rel(m), moi),
            _ => format!("[{}] {}", format_term(t), moi),
        },
        JboRel::OperatorRel(op) => format!("[{}]", format_operator(op)),
        JboRel::ScalarNegatedRel(nahe, r) => {
            format!("{{{}}}({})", nahe, format_scalar_negated_rel_inner(r))
        }
        JboRel::VPredRel(vpred) => format_seltau_prop(&vpred(&[JboTerm::BoundVar(0)])),
        JboRel::AbsPred(abs, npred) => {
            let args: Vec<JboTerm> = (1..=npred.arity as i32)
                .map(|n| JboTerm::BoundVar(-n))
                .collect();
            format!("{}[{}]", abs, format_lambda_prop(&(npred.pred)(&args)))
        }
        JboRel::AbsProp(abs, prop) => format!("{}[{}]", abs, format_prop(prop)),
        JboRel::TagRel(tag) => format!("{{{}}}", format_tag(tag)),
        JboRel::ModalRel(modal, r) => format_modal_rel(modal, r),
    }
}

// Ported from: JboShow.hs :: logjboshow for JboVPred with Bindful.hs :: withShuntedRelVar
// Helper for modal seltau inner proposition formatting
fn format_modal_seltau_inner_prop(prop: &JboProp) -> String {
    match prop {
        Prop::Rel(rel, terms) if terms.is_empty() => format!("{}()", format_rel(rel)),
        _ => format_seltau_prop(prop),
    }
}

// Ported from: JboShow.hs :: logjboshow for JboVPred with Bindful.hs :: withShuntedRelVar
// Formats seltau (tanru left component) with placeholder variable for logical output
fn format_seltau_prop(prop: &JboProp) -> String {
    match prop {
        Prop::Rel(JboRel::Among(t), terms)
            if terms.is_empty() || matches!(terms.as_slice(), [JboTerm::BoundVar(0)]) =>
        {
            format!("(_ ≤ {})", format_term(t))
        }
        Prop::Rel(JboRel::AppliedRel(r, _), terms)
            if terms.is_empty() && matches!(r.as_ref(), JboRel::Among(_)) =>
        {
            if let JboRel::Among(t) = r.as_ref() {
                format!("(_ ≤ {})", format_term(t))
            } else {
                unreachable!()
            }
        }
        Prop::Rel(rel, terms) if terms.is_empty() => format!("{}(_)", format_rel(rel)),
        Prop::Rel(rel, terms) => format!(
            "{}({})",
            format_rel(rel),
            terms
                .iter()
                .map(|term| {
                    if matches!(term, JboTerm::BoundVar(0)) {
                        "_".to_string()
                    } else {
                        format_term(term)
                    }
                })
                .collect::<Vec<_>>()
                .join(",")
        ),
        Prop::Modal(crate::jbo_prop::JboModalOp::Tagged(tag, mt), inner) => {
            let term = mt
                .as_ref()
                .map(|term| {
                    if matches!(term, JboTerm::BoundVar(0)) {
                        "_".to_string()
                    } else {
                        format_term(term)
                    }
                })
                .unwrap_or_default();
            format!(
                "({})({}). {}",
                format_tag(tag),
                term,
                format_modal_seltau_inner_prop(inner)
            )
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::WithEventAs(t), inner) => {
            format!("{}=. {}", format_term(t), format_seltau_prop(inner))
        }
        Prop::Modal(_, inner) => format_seltau_prop(inner),
        _ => "<vpred>".to_string(),
    }
}

// Ported from: JboShow.hs :: logjboshow for JboVPred with Bindful.hs :: withShuntedRelVar
// Formats seltau (tanru left component) with placeholder variable for logical output
fn format_seltau(rel: &JboRel) -> String {
    match rel {
        JboRel::Among(t) => format!("(_ ≤ {})", format_term(t)),
        JboRel::AppliedRel(r, _terms) if matches!(r.as_ref(), JboRel::Among(_)) => {
            if let JboRel::Among(t) = r.as_ref() {
                format!("(_ ≤ {})", format_term(t))
            } else {
                unreachable!()
            }
        }
        JboRel::ModalRel(_, _) => format_rel(rel),
        JboRel::AppliedRel(r, terms) => {
            format!(
                "{}({})",
                format_rel(r),
                terms
                    .iter()
                    .map(|term| {
                        if matches!(term, JboTerm::BoundVar(0)) {
                            "_".to_string()
                        } else {
                            format_term(term)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
        JboRel::VPredRel(vpred) => format_seltau_prop(&vpred(&[JboTerm::BoundVar(0)])),
        JboRel::Tanru(_, _) => {
            let inner = format_rel(rel);
            format!("{}(_)", inner)
        }
        _ => format!("{}(_)", format_rel(rel)),
    }
}

// Ported from: JboShow.hs :: instance JboShow JboProp (logshow method)
fn format_prop(
    prop: &Prop<
        JboRel,
        JboTerm,
        String,
        crate::jbo_prop::JboModalOp,
        crate::jbo_prop::JboQuantifier,
    >,
) -> String {
    format_prop_at(prop, 1)
}

// Ported from: JboParse.hs :: parseTag + JboProp.hs :: connToFol
// Expands connected logical tags into FOL connectives
fn expand_connected_tag_modal(
    tag: &crate::jbo_prop::JboTag,
    mt: &Option<JboTerm>,
    inner: &Prop<
        JboRel,
        JboTerm,
        String,
        crate::jbo_prop::JboModalOp,
        crate::jbo_prop::JboQuantifier,
    >,
) -> Option<
    Prop<JboRel, JboTerm, String, crate::jbo_prop::JboModalOp, crate::jbo_prop::JboQuantifier>,
> {
    if let crate::jbo_prop::JboTag::ConnectedTag(con, tag1, tag2) = tag {
        if let crate::jbo_prop::JboConnective::JboConnLog(_, lcon) = con.as_ref() {
            let left = Prop::Modal(
                crate::jbo_prop::JboModalOp::Tagged((**tag1).clone(), mt.clone()),
                Box::new(inner.clone()),
            );
            let right = Prop::Modal(
                crate::jbo_prop::JboModalOp::Tagged((**tag2).clone(), mt.clone()),
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

// Ported from: JboShow.hs :: logjboshow' Prop (main proposition formatting with variable tracking)
fn format_prop_at(
    prop: &Prop<
        JboRel,
        JboTerm,
        String,
        crate::jbo_prop::JboModalOp,
        crate::jbo_prop::JboQuantifier,
    >,
    next_var: i32,
) -> String {
    match prop {
        Prop::Rel(rel, terms) => {
            let terms = if matches!(rel, JboRel::ScalarNegatedRel(_, _)) {
                scalar_negated_terms(terms)
            } else {
                terms.clone()
            };
            // Special handling for fragments
            if let JboRel::Brivla(s) = rel {
                if s == "fragment" {
                    let terms_str = terms.iter().map(format_term).collect::<Vec<_>>().join(",");
                    return format!("[Fragment: {}]", terms_str);
                }
                if s == "unparsed-fragment" {
                    return "[Fragment]".to_string();
                }
            }

            // Ported from: JboShow.hs lines 598-604
            if matches!(rel, JboRel::Equal) {
                return format!(
                    "({})",
                    terms
                        .iter()
                        .map(format_term)
                        .collect::<Vec<_>>()
                        .join(" == ")
                );
            }
            if let JboRel::Among(t) = rel {
                if matches!(terms.as_slice(), [JboTerm::BoundVar(0)]) {
                    return format!("(_ ≤ {})", format_term(t));
                }
                let terms_str = terms.iter().map(format_term).collect::<Vec<_>>().join(",");
                return format!("({} ≤ {})", terms_str, format_term(t));
            }
            if let JboRel::AppliedRel(r, applied_terms) = rel {
                if let JboRel::Among(t) = r.as_ref() {
                    if terms.is_empty()
                        && matches!(applied_terms.as_slice(), [JboTerm::BoundVar(0)])
                    {
                        return format!("(_ ≤ {})", format_term(t));
                    }
                }
            }

            // Ported from: JboShow.hs lines 605-609
            // logjboshow' False [] (Rel r ts) =
            //     do s <- logshow r
            //        tss <- mapM logshow ts
            //        return $ [s ++ "(" ++ (intercalate "," tss) ++ ")"]
            let rel_str = format_rel(rel);
            let terms_str = terms.iter().map(format_term).collect::<Vec<_>>().join(",");
            format!("{}({})", rel_str, terms_str)
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::NonVeridical, inner) => {
            if let Prop::Quantified(q, restriction, body) = inner.as_ref() {
                let n = next_var;
                let mut prefix = match q {
                    JboQuantifier::RelQuantifier(inner_q) => {
                        format_quantified_prefix(inner_q, &format!("R{}", n))
                    }
                    _ => format_quantified_prefix(q, &format!("x{}", n)),
                };
                if let Some(r) = restriction {
                    prefix.push_str(":(");
                    prefix.push_str(&format_restriction_prop(&r(n), n));
                    prefix.push_str("). ");
                } else {
                    prefix.push_str(". ");
                }
                let body_next_var = if matches!(q, JboQuantifier::QuestionQuantifier) {
                    n
                } else {
                    n + 1
                };
                format!(
                    "non-veridical: {}{}",
                    prefix,
                    format_prop_at(&body(n), body_next_var)
                )
            } else {
                format!("non-veridical: {}", format_prop_at(inner, next_var))
            }
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::Tagged(tag, mt), inner) => {
            if let Some(expanded) = expand_connected_tag_modal(tag, mt, inner) {
                return format_prop_at(&expanded, next_var);
            }
            let term = mt.as_ref().map(format_term).unwrap_or_default();
            format!(
                "({})({}). {}",
                format_tag(tag),
                term,
                format_prop_at(inner, next_var)
            )
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::WithEventAs(t), inner) => {
            format!("{}=. {}", format_term(t), format_prop_at(inner, next_var))
        }
        Prop::Modal(crate::jbo_prop::JboModalOp::QTruthModal, inner) => {
            format!("?. {}", format_prop_at(inner, next_var))
        }
        Prop::Not(inner) => format!("¬{}", format_prop_at(inner, next_var)),
        Prop::Connected(conn, p1, p2) => {
            format!(
                "({} {} {})",
                format_prop_at(p1, next_var),
                conn,
                format_prop_at(p2, next_var)
            )
        }
        Prop::NonLogConnected(joik, p1, p2) => {
            format!(
                "({} {{{}}} {})",
                format_prop_at(p1, next_var),
                joik,
                format_prop_at(p2, next_var)
            )
        }
        Prop::Quantified(q, restriction, body) => {
            let n = next_var;
            let mut prefix = match q {
                JboQuantifier::RelQuantifier(inner_q) => {
                    format_quantified_prefix(inner_q, &format!("R{}", n))
                }
                _ => format_quantified_prefix(q, &format!("x{}", n)),
            };
            let body_prop = body(n);
            if let Some(r) = restriction {
                prefix.push_str(":(");
                prefix.push_str(&format_restriction_prop(&r(n), n));
                prefix.push_str("). ");
            } else {
                prefix.push_str(". ");
            }
            if let Prop::Modal(crate::jbo_prop::JboModalOp::NonVeridical, inner) = body_prop {
                format!("non-veridical: {}{}", prefix, format_prop_at(&inner, n + 1))
            } else {
                format!("{}{}", prefix, format_prop_at(&body_prop, n + 1))
            }
        }
        Prop::Eet => "_|_".to_string(),
    }
}

// Ported from: JboShow.hs :: jboshow (canonical output with .i statement joining)
fn push_canonical_line(lines: &mut Vec<String>, lojban: &str) {
    for line in lojban.lines().filter(|line| !line.is_empty()) {
        if lines.is_empty() || line.starts_with(".i ") {
            lines.push(line.to_string());
        } else {
            lines.push(format!(".i {}", line));
        }
    }
}

// Ported from: JboShow.hs :: instance JboShow Texticule (side texticule collection)
// Mirrors JboParse.hs :: replaceLastTermInProp storing discursive frees on TermWithSides
fn collect_term_side_texticules_from_texticule(texticule: &Texticule, out: &mut Vec<Texticule>) {
    match texticule {
        Texticule::TexticuleFrag(JboFragment::JboFragTerms(terms)) => {
            for term in terms {
                collect_term_side_texticules_from_term(term, out);
            }
        }
        Texticule::TexticuleProp(prop) => collect_term_side_texticules(prop, out),
        Texticule::TexticuleSide(_, inner) => {
            collect_term_side_texticules_from_texticule(inner, out)
        }
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
        JboTerm::PredNamed(pred) => collect_term_side_texticules(&pred(&JboTerm::Unfilled), out),
        JboTerm::Value(m) => collect_term_side_texticules_from_mex(m, out),
        JboTerm::QualifiedTerm(_, inner) => collect_term_side_texticules_from_term(inner, out),
        JboTerm::JoikedTerms(_, left, right) => {
            collect_term_side_texticules_from_term(left, out);
            collect_term_side_texticules_from_term(right, out);
        }
        JboTerm::JboQuote(_) => {}
        _ => {}
    }
}

// Rust adaptation: Helper for collect_term_side_texticules_from_term
fn collect_term_side_texticules_from_rel(rel: &JboRel, out: &mut Vec<Texticule>) {
    match rel {
        JboRel::Among(term) | JboRel::Moi(term, _) => {
            collect_term_side_texticules_from_term(term, out)
        }
        JboRel::Tanru(left, right) | JboRel::TanruConnective(_, left, right) => {
            collect_term_side_texticules_from_rel(left, out);
            collect_term_side_texticules_from_rel(right, out);
        }
        JboRel::AppliedRel(rel, terms) => {
            collect_term_side_texticules_from_rel(rel, out);
            for term in terms {
                collect_term_side_texticules_from_term(term, out);
            }
        }
        JboRel::PermutedRel(_, rel)
        | JboRel::ScalarNegatedRel(_, rel)
        | JboRel::ModalRel(_, rel) => {
            collect_term_side_texticules_from_rel(rel, out);
        }
        JboRel::OperatorRel(op) => collect_term_side_texticules_from_operator(op, out),
        JboRel::TagRel(tag) => collect_term_side_texticules_from_tag(tag, out),
        JboRel::AbsPred(_, npred) => {
            collect_term_side_texticules(&(npred.pred)(&vec![JboTerm::Unfilled; npred.arity]), out)
        }
        JboRel::AbsProp(_, prop) => collect_term_side_texticules(prop, out),
        JboRel::VPredRel(vpred) => collect_term_side_texticules(&vpred(&[JboTerm::Unfilled]), out),
        _ => {}
    }
}

// Rust adaptation: Helper for collect_term_side_texticules_from_rel
fn collect_term_side_texticules_from_mex(mex: &crate::jbo_prop::JboMex, out: &mut Vec<Texticule>) {
    match mex {
        crate::jbo_prop::JboMex::Operation(op, args) => {
            collect_term_side_texticules_from_operator(op, out);
            for arg in args {
                collect_term_side_texticules_from_mex(arg, out);
            }
        }
        crate::jbo_prop::JboMex::ConnectedMex(_, _, left, right) => {
            collect_term_side_texticules_from_mex(left, out);
            collect_term_side_texticules_from_mex(right, out);
        }
        crate::jbo_prop::JboMex::QualifiedMex(_, inner) => {
            collect_term_side_texticules_from_mex(inner, out)
        }
        crate::jbo_prop::JboMex::MexSelbri(vpred) => {
            collect_term_side_texticules(&vpred(&[JboTerm::Unfilled]), out)
        }
        crate::jbo_prop::JboMex::MexSumti(term) => {
            collect_term_side_texticules_from_term(term, out)
        }
        crate::jbo_prop::JboMex::MexArray(ms) => {
            for m in ms {
                collect_term_side_texticules_from_mex(m, out);
            }
        }
        _ => {}
    }
}

// Rust adaptation: Helper for collect_term_side_texticules_from_mex
fn collect_term_side_texticules_from_operator(
    op: &crate::jbo_prop::JboOperator,
    out: &mut Vec<Texticule>,
) {
    match op {
        crate::jbo_prop::JboOperator::ConnectedOperator(_, _, left, right) => {
            collect_term_side_texticules_from_operator(left, out);
            collect_term_side_texticules_from_operator(right, out);
        }
        crate::jbo_prop::JboOperator::OpPermuted(_, op)
        | crate::jbo_prop::JboOperator::OpScalarNegated(_, op) => {
            collect_term_side_texticules_from_operator(op, out);
        }
        crate::jbo_prop::JboOperator::OpMex(mex) => collect_term_side_texticules_from_mex(mex, out),
        crate::jbo_prop::JboOperator::OpSelbri(vpred) => {
            collect_term_side_texticules(&vpred(&[JboTerm::Unfilled]), out)
        }
        crate::jbo_prop::JboOperator::OpVUhU(_) => {}
    }
}

// Rust adaptation: Helper for collect_term_side_texticules_from_operator
fn collect_term_side_texticules_from_tag(tag: &crate::jbo_prop::JboTag, out: &mut Vec<Texticule>) {
    match tag {
        crate::jbo_prop::JboTag::DecoratedTagUnits(units) => {
            for unit in units {
                match &unit.tag_unit {
                    crate::jbo_prop::JboTagUnit::ROI(_, _, mex) => {
                        collect_term_side_texticules_from_mex(mex, out)
                    }
                    crate::jbo_prop::JboTagUnit::FIhO(vpred) => {
                        collect_term_side_texticules(&vpred(&[JboTerm::Unfilled]), out)
                    }
                    _ => {}
                }
            }
        }
        crate::jbo_prop::JboTag::ConnectedTag(_, left, right) => {
            collect_term_side_texticules_from_tag(left, out);
            collect_term_side_texticules_from_tag(right, out);
        }
    }
}

// Ported from: JboShow.hs :: jboshow for side texticules (sei/to markers)
fn jboshow_side_texticule(texticule: &Texticule) -> String {
    match texticule {
        Texticule::TexticuleSide(crate::jbo_prop::SideType::SideDiscursive, inner) => {
            format!("sei {} se'u", crate::jbo_show::jboshow_texticule(inner))
        }
        Texticule::TexticuleSide(crate::jbo_prop::SideType::SideBracketed, inner) => {
            format!("to {} toi", crate::jbo_show::jboshow_texticule(inner))
        }
        _ => crate::jbo_show::jboshow_texticule(texticule),
    }
}

// Rust-only: Helper to check if proposition starts with NonVeridical modal
fn is_leading_nonveridical_prop(prop: &JboProp) -> bool {
    match prop {
        Prop::Modal(crate::jbo_prop::JboModalOp::NonVeridical, _) => true,
        Prop::Connected(crate::logic::Connective::And, left, _) => {
            is_leading_nonveridical_prop(left)
        }
        _ => false,
    }
}

// Rust-only: Helper to count leading NonVeridical propositions for side insertion
fn leading_nonveridical_count(result: &SemanticResult) -> usize {
    result
        .side_props
        .iter()
        .filter(|prop| is_leading_nonveridical_prop(prop))
        .count()
        + usize::from(is_leading_nonveridical_prop(&result.prop))
}

// Ported from: JboShow.hs :: jboshow for fragment terms
fn push_fragment_terms_line(lines: &mut Vec<String>, terms: &[JboTerm]) {
    if !terms.is_empty() {
        let frag = terms
            .iter()
            .map(crate::jbo_show::jboshow_term)
            .collect::<Vec<_>>()
            .join(" ");
        push_canonical_line(lines, &frag);
    } else if !lines.is_empty() {
        lines.push(".i ".to_string());
    }
}

// Rust adaptation: Helper for collect_term_side_texticules_from_texticule
fn collect_term_side_texticules(prop: &JboProp, out: &mut Vec<Texticule>) {
    match prop {
        Prop::Rel(rel, terms) => {
            collect_term_side_texticules_from_rel(rel, out);
            for term in terms {
                collect_term_side_texticules_from_term(term, out);
            }
        }
        Prop::Not(inner) | Prop::Modal(_, inner) => collect_term_side_texticules(inner, out),
        Prop::Connected(_, left, right) | Prop::NonLogConnected(_, left, right) => {
            collect_term_side_texticules(left, out);
            collect_term_side_texticules(right, out);
        }
        Prop::Quantified(_, _, body) => {
            collect_term_side_texticules(&body(1), out);
        }
        Prop::Eet => {}
    }
}

// Ported from: JboShow.hs :: jboshow + logshow (CLI output formatting)
// Combines logical LogShow lines with canonical JboShow text
fn format_result(result: &SemanticResult, preserve_term_side_markers: bool) -> (String, String) {
    let mut logical_lines = vec![];
    let mut term_sides = Vec::new();
    collect_term_side_texticules(&result.prop, &mut term_sides);

    if result.side_texticules.is_empty() {
        for prop in &result.side_props {
            logical_lines.push(format_prop(prop));
        }
    } else {
        for texticule in &result.side_texticules {
            logical_lines.push(format_texticule(texticule));
        }
    }
    for texticule in &term_sides {
        logical_lines.push(format_texticule(texticule));
    }

    logical_lines.push(format_prop(&result.prop));

    let mut canonical_lines = Vec::new();
    if preserve_term_side_markers && !result.side_texticules.is_empty() {
        for texticule in &result.side_texticules {
            push_canonical_line(&mut canonical_lines, &jboshow_side_texticule(texticule));
        }
        match &result.prop {
            Prop::Rel(JboRel::Brivla(name), terms) if name == "fragment" => {
                push_fragment_terms_line(&mut canonical_lines, terms);
            }
            _ => push_canonical_line(
                &mut canonical_lines,
                &crate::jbo_show::prop_to_lojban(&result.prop),
            ),
        }
    } else {
        push_canonical_line(&mut canonical_lines, &result.lojban_output);
    }
    let side_lines = term_sides
        .into_iter()
        .flat_map(|texticule| {
            let shown = if preserve_term_side_markers {
                jboshow_side_texticule(&texticule)
            } else {
                crate::jbo_show::jboshow_texticule(&texticule)
            };
            shown
                .lines()
                .filter(|line| !line.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    if !side_lines.is_empty() {
        let insert_at = leading_nonveridical_count(result).min(canonical_lines.len());
        for (i, line) in side_lines.into_iter().enumerate() {
            let line = if insert_at + i == 0 || line.starts_with(".i ") {
                line
            } else {
                format!(".i {}", line)
            };
            canonical_lines.insert(insert_at + i, line);
        }
    }

    let logical = logical_lines.join("\n");
    let canonical = canonical_lines.join("\n");
    (logical, canonical)
}

// Ported from: Main.hs :: evalText output formatting
/// Return `(logical, canonical, graph_json)` for CLI/JSON output.
pub fn eval_text_to_outputs_with_options(
    text: &Text,
    include_term_sides: bool,
) -> (String, String, String) {
    eval_text_to_outputs_with_runtime_options(text, include_term_sides, false)
}

pub fn eval_text_to_outputs_with_runtime_options(
    text: &Text,
    include_term_sides: bool,
    indicator_texticules: bool,
) -> (String, String, String) {
    let results = eval_text_with_options(text, indicator_texticules);

    if results.is_empty() {
        return (String::new(), String::new(), "{}".to_string());
    }

    let mut logical_lines = Vec::new();
    let mut canonical_lines = Vec::new();

    for result in results.iter() {
        let (logical, canonical) = format_result(result, include_term_sides);
        logical_lines.push(logical);
        for line in canonical.lines().filter(|line| !line.is_empty()) {
            if canonical_lines.is_empty() || line.starts_with(".i ") {
                canonical_lines.push(line.to_string());
            } else {
                canonical_lines.push(format!(".i {}", line));
            }
        }
    }

    let logical = logical_lines.join("\n");
    let canonical = canonical_lines.join("\n");
    let mut props = Vec::new();
    for result in &results {
        props.extend(result.side_props.iter().cloned());
        let mut term_sides = Vec::new();
        collect_term_side_texticules(&result.prop, &mut term_sides);
        props.extend(
            term_sides
                .into_iter()
                .filter_map(|texticule| match texticule {
                    Texticule::TexticuleProp(prop) => Some(prop),
                    _ => None,
                }),
        );
        props.push(result.prop.clone());
    }
    let graph = crate::jbo_tree::jbo_props_to_graph(&props);
    let graph_json = serde_json::to_string(&graph).unwrap_or_else(|_| "{}".to_string());

    (logical, canonical, graph_json)
}

// Ported from: Main.hs :: evalText output formatting
pub fn eval_text_to_outputs(text: &Text) -> (String, String, String) {
    eval_text_to_outputs_with_options(text, false)
}

/// Return Prolog source code for the parsed text.
pub fn eval_text_to_prolog(text: &Text) -> String {
    let results = eval_text(text);
    crate::jbo_prolog::semantic_results_to_prolog(&results)
}
