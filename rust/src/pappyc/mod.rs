//! Rust port of the Haskell [`pappy/pappy`](../../pappy/pappy) compiler (`Main.hs`, `ReadGrammar.hs`,
//! `ReduceGrammar.hs`, `MemoAnalysis.hs`, `WriteParser.hs` → Rust emission).
//!
//! **Semantic actions** in `.pappy` are still written for the Haskell backend today. When using
//! `--emit rust`, action bodies are pasted **verbatim** into the generated Rust; migrate each
//! `{ ... }` block to valid Rust (or maintain a Rust-specific grammar) before the crate will compile.

pub mod ast;
pub mod error;
pub mod memo_analysis;
pub mod read_grammar;
pub mod reduce_grammar;
pub mod simplify_grammar;
pub mod write_parser_rust;

pub use ast::{Grammar, Identifier, Match, Nonterminal, Producer, RawCode, Rule};
pub use error::PappycError;

use std::path::Path;

/// Full pipeline: parse `.pappy`, reduce, (simplify), memo analysis, emit Rust source.
pub fn compile_to_rust(name: &str, source: &str) -> Result<String, PappycError> {
    let g = read_grammar::parse_grammar(name, source)?;
    let g = reduce_grammar::reduce_grammar(g)?;
    let g = simplify_grammar::simplify_grammar(g);
    let memos = memo_analysis::memo_analysis(&g.grammar_nonterminals);
    write_parser_rust::write_parser_rust(&memos, &g)
}

/// Load and compile a grammar file (`import "Other.pappy"` inclusion is not implemented yet).
#[cfg(test)]
mod tests {
    use super::*;

    const MIN_PAPPY: &str = r#"parser min:

{
}

top start

start :: () = "a"
"#;

    const STR_LIT_PAPPY: &str = r#"parser strlit:

{
}

top tok

tok :: String = "hi"
"#;

    const STR_ALT_PAPPY: &str = r#"parser stralt:

{
}

top s

a :: String = "x"
b :: String = "y"
s :: String = a / b
"#;

    /// Literal + `String` NT; `-> { … }` is ignored by Rust emitter (concat semantics).
    const STR_SEQ_PAPPY: &str = r#"parser strseq:

{
}

top s

tok :: String = "y"
s :: String = "x" tok -> { "x" ++ tok }
"#;

    /// `String` `RuleSeq` with **`&`** on a **`()`** NT then a literal (concat from literal only).
    const STR_SEQ_AND_UNIT_PAPPY: &str = r#"parser strpeek:

{
}

top s

peek :: () = "x"
s :: String = &peek "hi" -> { "hi" }
"#;

    /// `&` on a **`String`** NT, then literal.
    const STR_SEQ_AND_STRING_PAPPY: &str = r#"parser strandtok:

{
}

top s

tok :: String = "hi"
s :: String = &tok " there" -> { "hi" ++ " there" }
"#;

    /// **`!`** on a literal, then literal (`String` concat).
    const STR_SEQ_NOT_LIT_PAPPY: &str = r#"parser strnotlit:

{
}

top s

s :: String = !"bad" "ok" -> { "ok" }
"#;

    const FWD_PAPPY: &str = r#"parser fwd:

{
}

top main

main :: () = leaf
leaf :: () = "x"
"#;

    /// `RulePrim` forward with **`-> rhs`** (`ProdName`), not only **`-> ()`**.
    const FWD_NAMED_PAPPY: &str = r#"parser fwn:

{
}

top main

other :: () = "x"
main :: () = other -> rhs
"#;

    /// **`String`** `RulePrim` forward with **`-> rhs`** (`ProdName`).
    const FWD_STRING_NAMED_PAPPY: &str = r#"parser fwsn:

{
}

top main

tok :: String = "hi"
main :: String = tok -> rhs
"#;

    const ALT_PAPPY: &str = r#"parser altg:

{
}

top start

start :: () = "a" / "b"
"#;

    const ALT_MIX_PAPPY: &str = r#"parser altm:

{
}

top s

a :: () = "x"
b :: () = "y"
s :: () = a / b
"#;

    /// `RuleAlt` branch that is a full **`()`** **`RuleSeq`** (not only literal / prim).
    const ALT_UNIT_SEQ_PAPPY: &str = r#"parser altuseq:

{
}

top s

peek :: () = "x"
s :: () = "a" / &peek "x" -> ()
"#;

    /// **`String` `RuleAlt`** with a branch that is a **`String` `RuleSeq`** (e.g. **`&`** + literal).
    const ALT_STRING_SEQ_PAPPY: &str = r#"parser altstrseq:

{
}

top s

tok :: String = "y"
s :: String = "p" / &tok "q" -> { "pq" }
"#;

    /// Literal + NT + `-> ()` (required by `read_grammar` so the rule is a `RuleSeq`, not unary fallback).
    const MIX_SEQ_PAPPY: &str = r#"parser mix:

{
}

top main

main :: () = "a" leaf -> ()
leaf :: () = "x"
"#;

    /// `()` **`RuleSeq`** with **`-> rhs`** (`ProdName`) — same codegen as `-> ()`; actions not translated.
    const UNIT_SEQ_PRODNAME_PAPPY: &str = r#"parser useqname:

{
}

top main

leaf :: () = "x"
main :: () = "a" leaf -> rhs
"#;

    const MIX_NOT_SEQ_PAPPY: &str = r#"parser nseq:

{
}

top main

bad :: () = "x"
main :: () = !bad "a" -> ()
"#;

    const MIX_AND_SEQ_PAPPY: &str = r#"parser aseq:

{
}

top main

peek :: () = "x"
main :: () = &peek "x" -> ()
"#;

    const MIX_AND_STRING_PAPPY: &str = r#"parser asstr:

{
}

top main

tok :: String = "hi"
main :: () = &tok "hi" -> ()
"#;

    const MIX_NOT_STRING_PAPPY: &str = r#"parser nsstr:

{
}

top main

tok :: String = "hi"
main :: () = !tok "x" -> ()
"#;

    const OPT_PAPPY: &str = r#"parser optp:

{
}

top s

s :: () = "a"? -> ()
"#;

    const OPT_PRIM_PAPPY: &str = r#"parser opt2:

{
}

top s

s :: () = leaf? -> ()
leaf :: () = "x"
"#;

    /// Postfix `?` with outer **`-> rhs`** (`ProdName`), not only **`-> ()`**.
    const OPT_UNIT_PRODNAME_PAPPY: &str = r#"parser optname:

{
}

top s

leaf :: () = "x"
s :: () = leaf? -> rhs
"#;

    /// Postfix `?` on a parenthesized supported **`()`** **`RuleSeq`** (`RuleOpt` wraps inner `RuleSeq`).
    const OPT_UNIT_SEQ_PAPPY: &str = r#"parser optuseq:

{
}

top s

peek :: () = "x"
s :: () = (&peek "a" -> ())? -> ()
"#;

    /// Postfix `?` on a parenthesized supported **`String` `RuleSeq`**.
    const OPT_STRING_SEQ_PAPPY: &str = r#"parser optstrseq:

{
}

top s

tok :: String = "y"
s :: String = (&tok "z" -> { "" })? -> { "" }
"#;

    const UNIT_STAR_PAPPY: &str = r#"parser ustar:

{
}

top s

s :: () = "a"* -> ()
"#;

    const UNIT_PLUS_PAPPY: &str = r#"parser uplus:

{
}

top s

s :: () = "x"+ -> ()
"#;

    /// `[String]` from reduce `StarRule` / `many` — inner must be `String` NT or literal (not bare `*` of `()` only).
    const STR_LIST_STAR_PAPPY: &str = r#"parser sstr:

{
}

top toks

tok :: String = "a"
toks :: {[String]} = tok*
"#;

    const STR_LIST_STAR_DISCARD_PAPPY: &str = r#"parser sstrd:

{
}

top s

tok :: String = "a"
toks :: {[String]} = tok*
s :: () = toks -> ()
"#;

    const STR_OPT_LIT_PAPPY: &str = r#"parser strop:

{
}

top s

s :: String = "a"?
"#;

    const STR_OPT_PRIM_PAPPY: &str = r#"parser strop2:

{
}

top s

s :: String = leaf? -> ()
leaf :: String = "x"
"#;

    #[test]
    fn compile_min_grammar() {
        let out = compile_to_rust("min.pappy", MIN_PAPPY).expect("compile");
        assert!(out.contains("pub struct Min"));
        assert!(out.contains("pub min_text: String"), "derivs must hold remaining input");
        assert!(
            out.contains("end of input"),
            "dv_char must match Haskell empty-input error"
        );
        assert!(
            out.contains("fn min_parse_start") && out.contains("string_("),
            "top rule `start` should get a real () parser for literal \"a\""
        );
        assert!(out.contains("fn min_parse(") && out.contains("fn min_derivs("));
    }

    #[test]
    fn compile_file_to_rust_smoke() {
        let path = std::env::temp_dir().join(format!(
            "tersmu_compile_file_test_{}.pappy",
            std::process::id()
        ));
        std::fs::write(&path, MIN_PAPPY).expect("write temp .pappy");
        let out = super::compile_file_to_rust(&path).expect("compile_file_to_rust");
        let _ = std::fs::remove_file(&path);
        assert!(
            out.contains("pub struct Min"),
            "compile_file_to_rust should match compile_to_rust for the same bytes"
        );
    }

    #[test]
    fn compile_to_rust_rejects_invalid_grammar() {
        let err = compile_to_rust("bad.pappy", "not a pappy grammar").unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("bad.pappy") && msg.to_ascii_lowercase().contains("parse"),
            "expected parse error in Display: {msg}"
        );
    }

    #[test]
    fn compile_file_to_rust_missing_file_is_io_error() {
        let path = std::env::temp_dir().join(format!(
            "tersmu_missing_grammar_{}.pappy",
            std::process::id()
        ));
        let err = super::compile_file_to_rust(&path).unwrap_err();
        match err {
            PappycError::Io { path: p, .. } => {
                assert!(
                    p.contains("tersmu_missing_grammar") || p.ends_with(".pappy"),
                    "unexpected Io path: {p}"
                );
            }
            e => panic!("expected PappycError::Io, got {e:?}"),
        }
    }

    #[test]
    fn compile_string_literal_grammar() {
        let out = compile_to_rust("strlit.pappy", STR_LIT_PAPPY).expect("compile");
        assert!(
            out.contains("fn strlit_parse_tok")
                && out.contains("string_value")
                && out.contains("PResult<Strlit, String>"),
            "String-typed literal rule should use string_value and return String"
        );
    }

    #[test]
    fn compile_string_alt_grammar() {
        let out = compile_to_rust("stralt.pappy", STR_ALT_PAPPY).expect("compile");
        assert!(
            out.contains("fn stralt_parse_s")
                && out.contains("string_value(\"x\")")
                && out.contains("stralt_parse_a")
                && out.contains("choice("),
            "String RuleAlt should mix string_value and NT parsers"
        );
    }

    #[test]
    fn compile_string_seq_grammar() {
        let out = compile_to_rust("strseq.pappy", STR_SEQ_PAPPY).expect("compile");
        assert!(
            out.contains("fn strseq_parse_s")
                && out.contains("acc_s.push_str")
                && out.contains("strseq_parse_tok")
                && out.contains("PResult<Strseq, String>"),
            "String RuleSeq should concat literals and String NT parses"
        );
    }

    #[test]
    fn compile_string_seq_and_unit_grammar() {
        let out = compile_to_rust("strpeek.pappy", STR_SEQ_AND_UNIT_PAPPY).expect("compile");
        assert!(
            out.contains("followed_by")
                && out.contains("strpeek_parse_peek")
                && out.contains("string_(\"hi\")")
                && out.contains("acc_s.push_str"),
            "String RuleSeq: & on () NT should use followed_by then literal concat"
        );
    }

    #[test]
    fn compile_string_seq_and_string_prim_grammar() {
        let out = compile_to_rust("strandtok.pappy", STR_SEQ_AND_STRING_PAPPY).expect("compile");
        assert!(
            out.contains("followed_by")
                && out.contains("strandtok_parse_tok")
                && out.contains("string_(\" there\")"),
            "String RuleSeq: & on String NT should use followed_by(Parser::new(...parse tok...))"
        );
    }

    #[test]
    fn compile_string_seq_not_literal_grammar() {
        let out = compile_to_rust("strnotlit.pappy", STR_SEQ_NOT_LIT_PAPPY).expect("compile");
        assert!(
            out.contains("not_followed_by(string_(\"bad\"))")
                && out.contains("string_(\"ok\")")
                && out.contains("acc_s.push_str"),
            "String RuleSeq: !literal should use not_followed_by then concat"
        );
    }

    #[test]
    fn compile_string_opt_literal_grammar() {
        let out = compile_to_rust("strop.pappy", STR_OPT_LIT_PAPPY).expect("compile");
        assert!(
            out.contains("fn strop_parse_s")
                && out.contains("opt_string_default")
                && out.contains("string_value(\"a\")")
                && out.contains("PResult<Strop, String>"),
            "String RuleOpt literal should use opt_string_default(string_value(...))"
        );
    }

    #[test]
    fn compile_string_opt_prim_grammar() {
        let out = compile_to_rust("strop2.pappy", STR_OPT_PRIM_PAPPY).expect("compile");
        assert!(
            out.contains("fn strop2_parse_s")
                && out.contains("opt_string_default")
                && out.contains("strop2_parse_leaf")
                && out.contains("PResult<Strop2, String>"),
            "String RuleOpt over String NT should use opt_string_default(Parser::new(...))"
        );
    }

    #[test]
    fn compile_forward_prim_grammar() {
        let out = compile_to_rust("fwd.pappy", FWD_PAPPY).expect("compile");
        assert!(
            out.contains("fwd_parse_main") && out.contains("fwd_parse_leaf"),
            "RulePrim closure should emit callee parser"
        );
        assert!(
            out.contains("fwd_parse_leaf(d)") || out.contains("fwd_parse_leaf (d)"),
            "main should forward to leaf"
        );
    }

    #[test]
    fn compile_forward_prim_named_producer_grammar() {
        let out = compile_to_rust("fwn.pappy", FWD_NAMED_PAPPY).expect("compile");
        assert!(
            out.contains("fn fwn_parse_main") && out.contains("fwn_parse_other(d)"),
            "RulePrim forward should accept -> name producer"
        );
    }

    #[test]
    fn compile_forward_prim_string_named_producer_grammar() {
        let out = compile_to_rust("fwsn.pappy", FWD_STRING_NAMED_PAPPY).expect("compile");
        assert!(
            out.contains("fn fwsn_parse_main")
                && out.contains("PResult<Fwsn, String>")
                && out.contains("fwsn_parse_tok(d)"),
            "String RulePrim forward should accept -> name producer"
        );
    }

    #[test]
    fn compile_unit_alt_literal_grammar() {
        let out = compile_to_rust("altg.pappy", ALT_PAPPY).expect("compile");
        assert!(
            out.contains("choice(string_(") && out.contains("\"a\"") && out.contains("\"b\""),
            "literal alternation should use choice + string_"
        );
    }

    #[test]
    fn compile_unit_alt_prim_branches_grammar() {
        let out = compile_to_rust("altm.pappy", ALT_MIX_PAPPY).expect("compile");
        assert!(
            out.contains("altm_parse_a") && out.contains("altm_parse_b"),
            "alternation of NTs should reference each branch parser"
        );
        assert!(
            out.contains("choice(") && out.contains("Parser::new"),
            "NT branches should use Parser::new around callees"
        );
    }

    #[test]
    fn compile_unit_alt_rule_seq_branch_grammar() {
        let out = compile_to_rust("altuseq.pappy", ALT_UNIT_SEQ_PAPPY).expect("compile");
        assert!(
            out.contains("choice(")
                && out.contains("string_(\"a\")")
                && out.contains("Parser::new(|dvs: Altuseq")
                && out.contains("followed_by")
                && out.contains("altuseq_parse_peek")
                && out.contains("string_(\"x\")"),
            "() RuleAlt may include a branch emitted as Parser::new closure over a full RuleSeq body"
        );
    }

    #[test]
    fn compile_string_alt_rule_seq_branch_grammar() {
        let out = compile_to_rust("altstrseq.pappy", ALT_STRING_SEQ_PAPPY).expect("compile");
        assert!(
            out.contains("choice(")
                && out.contains("string_value(\"p\")")
                && out.contains("Parser::new(|dvs: Altstrseq")
                && out.contains("followed_by")
                && out.contains("altstrseq_parse_tok")
                && out.contains("string_(\"q\")"),
            "String RuleAlt may include a branch emitted as Parser::new closure over a full String RuleSeq body"
        );
    }

    #[test]
    fn compile_mix_literal_prim_seq_grammar() {
        let out = compile_to_rust("mix.pappy", MIX_SEQ_PAPPY).expect("compile");
        assert!(
            out.contains("mix_parse_leaf(&cur)") && out.contains("string_(\"a\")"),
            "mixed () RuleSeq should chain string_ and callee parsers"
        );
    }

    #[test]
    fn compile_unit_rule_seq_named_producer_grammar() {
        let out = compile_to_rust("useqname.pappy", UNIT_SEQ_PRODNAME_PAPPY).expect("compile");
        assert!(
            out.contains("useqname_parse_main")
                && out.contains("string_(\"a\")")
                && out.contains("useqname_parse_leaf(&cur)"),
            "() RuleSeq with -> name should emit like -> (); producer ignored"
        );
    }

    #[test]
    fn compile_unit_not_mix_seq_grammar() {
        let out = compile_to_rust("nseq.pappy", MIX_NOT_SEQ_PAPPY).expect("compile");
        assert!(
            out.contains("not_followed_by")
                && out.contains("nseq_parse_bad")
                && out.contains("string_(\"a\")"),
            "() RuleSeq with ! should use not_followed_by on callee then string_"
        );
    }

    #[test]
    fn compile_unit_and_mix_seq_grammar() {
        let out = compile_to_rust("aseq.pappy", MIX_AND_SEQ_PAPPY).expect("compile");
        assert!(
            out.contains("followed_by")
                && out.contains("aseq_parse_peek")
                && out.contains("string_(\"x\")"),
            "() RuleSeq with & should use followed_by on callee then string_"
        );
    }

    #[test]
    fn compile_unit_and_string_prim_seq_grammar() {
        let out = compile_to_rust("asstr.pappy", MIX_AND_STRING_PAPPY).expect("compile");
        assert!(
            out.contains("followed_by")
                && out.contains("asstr_parse_tok")
                && out.contains("string_(\"hi\")"),
            "& on String NT should use followed_by(Parser::new(...parse tok...))"
        );
    }

    #[test]
    fn compile_unit_not_string_prim_seq_grammar() {
        let out = compile_to_rust("nsstr.pappy", MIX_NOT_STRING_PAPPY).expect("compile");
        assert!(
            out.contains("not_followed_by")
                && out.contains("nsstr_parse_tok")
                && out.contains("string_(\"x\")"),
            "! on String NT should use not_followed_by(Parser::new(...parse tok...))"
        );
    }

    #[test]
    fn compile_unit_opt_literal_grammar() {
        let out = compile_to_rust("optp.pappy", OPT_PAPPY).expect("compile");
        assert!(
            out.contains("opt_ignore(string_("),
            "postfix ? on literal should use opt_ignore(string_(…))"
        );
    }

    #[test]
    fn compile_unit_opt_prim_grammar() {
        let out = compile_to_rust("opt2.pappy", OPT_PRIM_PAPPY).expect("compile");
        assert!(
            out.contains("opt_ignore(Parser::new") && out.contains("opt2_parse_leaf"),
            "postfix ? on unit NT should wrap Parser::new around callee"
        );
    }

    #[test]
    fn compile_unit_opt_named_producer_grammar() {
        let out = compile_to_rust("optname.pappy", OPT_UNIT_PRODNAME_PAPPY).expect("compile");
        assert!(
            out.contains("opt_ignore(Parser::new") && out.contains("optname_parse_leaf"),
            "postfix ? with -> name on outer RuleSeq should unwrap like -> ()"
        );
    }

    #[test]
    fn compile_unit_opt_rule_seq_grammar() {
        let out = compile_to_rust("optuseq.pappy", OPT_UNIT_SEQ_PAPPY).expect("compile");
        assert!(
            out.contains("opt_ignore(Parser::new")
                && out.contains("optuseq_parse_peek")
                && out.contains("followed_by")
                && out.contains("string_(\"a\")"),
            "postfix ? on () RuleSeq should use opt_ignore(Parser::new(RuleSeq body))"
        );
    }

    #[test]
    fn compile_string_opt_rule_seq_grammar() {
        let out = compile_to_rust("optstrseq.pappy", OPT_STRING_SEQ_PAPPY).expect("compile");
        assert!(
            out.contains("opt_string_default(Parser::new")
                && out.contains("optstrseq_parse_tok")
                && out.contains("string_(\"z\")"),
            "postfix ? on String RuleSeq should use opt_string_default(Parser::new(RuleSeq body))"
        );
    }

    #[test]
    fn compile_unit_star_grammar() {
        let out = compile_to_rust("ustar.pappy", UNIT_STAR_PAPPY).expect("compile");
        assert!(
            out.contains("fn ustar_parse_s")
                && out.contains("map(many(string_(\"a\"))")
                && out.contains("PResult<Ustar, ()>"),
            "reduced StarRuleN should emit many(string_(…)); top () rule discards with map"
        );
    }

    #[test]
    fn compile_unit_plus_grammar() {
        let out = compile_to_rust("uplus.pappy", UNIT_PLUS_PAPPY).expect("compile");
        assert!(
            out.contains("fn uplus_parse_s")
                && out.contains("map(many1(string_(\"x\"))")
                && out.contains("PResult<Uplus, ()>"),
            "reduced PlusRuleN should emit many1(string_(…)); top () rule discards with map"
        );
    }

    #[test]
    fn compile_unit_plus_prim_grammar() {
        const PAPPY: &str = r#"parser upl:

{
}

top s

s :: () = leaf+ -> ()
leaf :: () = "y"
"#;
        let out = compile_to_rust("upl.pappy", PAPPY).expect("compile");
        assert!(
            out.contains("fn upl_parse_s")
                && out.contains("map(many1(")
                && out.contains("upl_parse_leaf")
                && out.contains("PResult<Upl, ()>"),
            "RulePlus on unit NT should use map(many1(Parser::new(...)), |_| ())"
        );
    }

    #[test]
    fn compile_string_list_star_grammar() {
        let out = compile_to_rust("sstr.pappy", STR_LIST_STAR_PAPPY).expect("compile");
        assert!(
            out.contains("fn sstr_parse_toks")
                && out.contains("map(many(")
                && out.contains("sstr_parse_tok"),
            "[String] StarRule should use many(Parser::new(... sstr_parse_tok ...))"
        );
        assert!(
            out.contains("PResult<Sstr, Vec<String>>"),
            "toks should return Vec<String>"
        );
    }

    #[test]
    fn compile_string_list_star_discard_grammar() {
        let out = compile_to_rust("sstrd.pappy", STR_LIST_STAR_DISCARD_PAPPY).expect("compile");
        assert!(
            out.contains("fn sstrd_parse_s") && out.contains("map(Parser::new"),
            "() = toks -> () should discard Vec<String> parse"
        );
        assert!(
            out.contains("many(") && out.contains("sstrd_parse_toks"),
            "list NT should still use many(...)"
        );
    }

    /// Regression guard: canonical repo grammars must parse, reduce, memoize, and emit without panic.
    #[test]
    fn compile_morphology_pappy_smoke() {
        let src = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../Morphology.pappy"));
        let out = compile_to_rust("Morphology.pappy", src).expect("compile morphology");
        assert!(
            out.contains("pub struct Morphology"),
            "expected Morphology derivs struct"
        );
        assert!(
            out.contains("fn morphology_parse(") && out.contains("fn morphology_derivs("),
            "expected morphology entrypoints"
        );
    }

    /// `read_grammar` must accept string literals without `"lit":rule` (e.g. `"%%%END%%%"`).
    #[test]
    fn parse_string_literal_without_colon_rule() {
        let g = r#"parser test:
{
}
top t
x :: () = "%%%END%%%" -> ()
"#;
        compile_to_rust("t.pappy", g).expect("plain string literal in a rule should parse and reduce");
    }

    #[test]
    fn compile_lojban_pappy_smoke() {
        let src = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../Lojban.pappy"));
        let out = compile_to_rust("Lojban.pappy", src).expect("compile lojban");
        assert!(out.contains("pub struct Lojban"), "expected Lojban derivs struct");
        assert!(
            out.contains("fn lojban_parse(") && out.contains("fn lojban_derivs("),
            "expected lojban entrypoints"
        );
    }
}

pub fn compile_file_to_rust(path: &Path) -> Result<String, PappycError> {
    let source = std::fs::read_to_string(path).map_err(|e| PappycError::Io {
        path: path.display().to_string(),
        message: e.to_string(),
    })?;
    let name = path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("grammar.pappy");
    compile_to_rust(name, &source)
}
