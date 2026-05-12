//! Semantic analysis from JboSyntax to JboProp - COMPLETE PORT
//!
//! Ported from: JboParse.hs
//!
//! This is a complete port of JboParse.hs that handles:
//! - Modal operators (PU, ZI, VA, etc.)
//! - Tags and FA terms
//! - Connectives (logical and non-logical)
//! - Quantifiers (ro, su'o, pa, re, etc.)
//! - Relative clauses (poi, noi)
//! - Abstractions (nu, ka, du'u)
//! - Complex tanru
//! - Variable binding and scope
//!
//! The full semantic pipeline processes Text -> Statement -> Sentence -> BridiTail -> Selbri -> Terms

use crate::jbo_syntax::*;
use crate::jbo_prop::{JboFragment, JboRel, JboTerm, JboModalOp, JboQuantifier, JboPred, JboMex, JboOperator, Texticule};
use crate::logic::Prop;
use crate::parse_m::{ParseState, VariableDomain, Bridi, PreProp, jbo_rel_to_bridi, bridi_to_jbo_vpred, apply_transform_list_to_bridi, apply_transform_list};

pub type JboProp = Prop<JboRel, JboTerm, String, JboModalOp, JboQuantifier>;

// Ported from: JboParse.hs :: withQuestionsScoped
/// Scope questions within a computation, applying them at the end
fn with_questions_scoped<P, F>(top: bool, state: &mut ParseState, f: F) -> Result<P, String>
where
    P: PreProp,
    F: FnOnce(&mut ParseState) -> Result<P, String>,
{
    let saved_questions = state.questions.clone();
    state.questions.clear();
    let result = f(state)?;
    let result = crate::parse_m::do_questions(top, state, result);
    let remaining_questions = std::mem::take(&mut state.questions);
    state.questions = saved_questions;
    state.questions.extend(remaining_questions);
    Ok(result)
}

// Ported from: JboParse.hs :: withQuestionsScoped (bridi version)
/// Scope questions within a bridi computation
fn with_questions_scoped_bridi<F>(top: bool, state: &mut ParseState, f: F) -> Result<Bridi, String>
where
    F: FnOnce(&mut ParseState) -> Result<Bridi, String>,
{
    let saved_questions = state.questions.clone();
    state.questions.clear();
    let bridi = f(state)?;
    let qinfos = crate::parse_m::de_kau(top, state);
    let remaining_questions = std::mem::take(&mut state.questions);
    state.questions = saved_questions;
    state.questions.extend(remaining_questions);
    Ok(Box::new(move |args| {
        qinfos.iter().cloned().fold(bridi(args), |prop, qinfo| {
            crate::parse_m::do_q_info(qinfo, prop)
        })
    }))
}

// Ported from: JboParse.hs :: JboRelClause
/// Relative clause in semantic representation
#[derive(Clone)]
pub enum JboRelClause {
    JRRestrictive(JboPred),
    JRIncidental(JboPred),
    JRAssign(Result<SumtiAtom, JboTerm>),
}

// Ported from: JboParse.hs :: replaceLastTermInProp
/// Replace the last term of the main (leftmost) Rel in a proposition
pub fn replace_last_term_in_prop<F>(f: std::rc::Rc<F>, prop: JboProp) -> JboProp
where
    F: Fn(JboTerm) -> JboTerm + 'static,
{
    replace_last_term_in_prop_with_status(f, prop).0
}

// Ported from: JboParse.hs :: replaceLastTermInProp (internal helper with status tracking)
fn replace_last_term_in_prop_with_status<F>(f: std::rc::Rc<F>, prop: JboProp) -> (JboProp, bool)
where
    F: Fn(JboTerm) -> JboTerm + 'static,
{
    match prop {
        Prop::Eet => (Prop::Eet, false),
        Prop::Not(p) => {
            let (p, changed) = replace_last_term_in_prop_with_status(f, *p);
            (Prop::Not(Box::new(p)), changed)
        }
        Prop::Connected(c, p1, p2) => {
            let (p1, changed1) = replace_last_term_in_prop_with_status(f.clone(), *p1);
            let (p2, changed2) = replace_last_term_in_prop_with_status(f, *p2);
            (Prop::Connected(c, Box::new(p1), Box::new(p2)), changed1 || changed2)
        }
        Prop::NonLogConnected(c, p1, p2) => {
            let (p1, changed1) = replace_last_term_in_prop_with_status(f.clone(), *p1);
            let (p2, changed2) = replace_last_term_in_prop_with_status(f, *p2);
            (Prop::NonLogConnected(c, Box::new(p1), Box::new(p2)), changed1 || changed2)
        }
        Prop::Modal(o, p) => {
            let (p, changed) = replace_last_term_in_prop_with_status(f, *p);
            (Prop::Modal(o, Box::new(p)), changed)
        }
        Prop::Quantified(q, restriction, body) => {
            let changed = std::rc::Rc::new(std::cell::Cell::new(false));
            let restriction = restriction.map(|restriction| {
                let f = f.clone();
                let changed = changed.clone();
                std::rc::Rc::new(move |v| {
                    let (prop, did_change) = replace_last_term_in_prop_with_status(f.clone(), restriction(v));
                    changed.set(changed.get() || did_change);
                    prop
                }) as std::rc::Rc<dyn Fn(i32) -> JboProp>
            });
            let body = {
                let f = f.clone();
                let changed = changed.clone();
                std::rc::Rc::new(move |v| {
                    let (prop, did_change) = replace_last_term_in_prop_with_status(f.clone(), body(v));
                    changed.set(changed.get() || did_change);
                    prop
                }) as std::rc::Rc<dyn Fn(i32) -> JboProp>
            };
            let prop = Prop::Quantified(q, restriction, body);
            let _ = match &prop {
                Prop::Quantified(_, restriction, body) => {
                    if let Some(restriction) = restriction {
                        restriction(1);
                    }
                    body(1);
                }
                _ => {}
            };
            (prop, changed.get())
        }
        Prop::Rel(r, mut ts) => {
            if !ts.is_empty() {
                let last = ts.pop().unwrap();
                ts.push(f(last));
                (Prop::Rel(r, ts), true)
            } else {
                (Prop::Rel(r, ts), false)
            }
        }
    }
}

// Ported from: JboParse.hs :: mexExists, mexForall
/// Standard quantifier mex values
pub fn mex_exists() -> Mex {
    use crate::jbo_syntax::{Mex, Numeral};
    Mex::MexNumeralString(vec![Numeral::PA("su'o".to_string())])
}

// Ported from: JboParse.hs :: mexForall
pub fn mex_forall() -> Mex {
    use crate::jbo_syntax::{Mex, Numeral};
    Mex::MexNumeralString(vec![Numeral::PA("ro".to_string())])
}

// Ported from: JboParse.hs :: nullMex
pub fn null_mex() -> Mex {
    use crate::jbo_syntax::{Mex, Numeral};
    Mex::MexNumeralString(vec![Numeral::PA("tu'o".to_string())])
}

// Ported from: JboParse.hs :: terpJboMexAsQuantifier
/// Interpret a JboMex as a quantifier
// Helper: Interpret a JboMex (from jbo_prop) as a quantifier
fn terp_jbo_mex_as_quantifier_from_jbo(m: &JboMex) -> JboQuantifier {
    use crate::jbo_prop::JboMex;

    if let JboMex::MexNumeralString(numerals) = m {
        if numerals.len() == 1 {
            if let crate::jbo_syntax::Numeral::PA(cmavo) = &numerals[0] {
                match cmavo.as_str() {
                    "ro" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Forall),
                    "su'o" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists),
                    "no" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(0)),
                    "pa" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(1)),
                    "re" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(2)),
                    "ci" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(3)),
                    "vo" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(4)),
                    "mu" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(5)),
                    "xa" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(6)),
                    "ze" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(7)),
                    "bi" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(8)),
                    "so" => return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(9)),
                    _ => {}
                }
            }
        }
    }

    if let JboMex::MexInt(n) = m {
        return JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exactly(*n));
    }

    JboQuantifier::MexQuantifier(m.clone())
}

// Ported from: JboParse.hs :: nonveridicialPred
/// Wrap a predicate in a non-veridical modal
pub fn nonveridicial_pred(p: crate::jbo_prop::JboPred) -> crate::jbo_prop::JboPred {
    std::sync::Arc::new(move |t: &JboTerm| {
        Prop::Modal(JboModalOp::NonVeridical, Box::new(p(t)))
    })
}

// Ported from: JboParse.hs :: isDiscursiveFree
/// Check if a Free is a Discursive (SEI) free
pub fn is_discursive_free(free: &Free) -> bool {
    matches!(free, Free::Discursive(_))
}

// Ported from: JboParse.hs :: tuIsBuha
/// Check if a TanruUnit is a bu'a/bu'e/bu'i variable
pub fn tu_is_buha(tu: &TanruUnit) -> bool {
    if let TanruUnit::TUGOhA(g, _) = tu {
        g == "bu'a" || g == "bu'e" || g == "bu'i"
    } else {
        false
    }
}

// Ported from: JboParse.hs :: statementFrees
/// Extract all frees from a statement
pub fn statement_frees(stmt: &Statement) -> Vec<Free> {
    let mut frees = stmt.frees.clone();
    frees.extend(statement1_frees(&stmt.body));
    frees
}

// Ported from: JboParse.hs :: statement1Frees
/// Extract frees from a Statement1
pub fn statement1_frees(stmt1: &Statement1) -> Vec<Free> {
    match stmt1 {
        Statement1::StatementSentence { frees, .. } => frees.clone(),
        Statement1::ConnectedStatement(_, s1, s2) => {
            let mut result = statement1_frees(s1);
            result.extend(statement1_frees(s2));
            result
        }
        Statement1::StatementParas { .. } => vec![],
    }
}

// Ported from: JboParse.hs :: stripForeRestrictives
/// Strip leading restrictive relative clauses from a list
pub fn strip_fore_restrictives(rels: &[JboRelClause]) -> (Vec<&JboPred>, Vec<JboRelClause>) {
    let mut split = 0;
    while matches!(rels.get(split), Some(JboRelClause::JRRestrictive(_))) {
        split += 1;
    }

    let restrictives = rels[..split]
        .iter()
        .filter_map(|rel| match rel {
            JboRelClause::JRRestrictive(pred) => Some(pred),
            _ => None,
        })
        .collect();
    (restrictives, rels[split..].to_vec())
}

// Ported from: JboParse.hs :: segregateRels
/// Segregate relative clauses into restrictive, incidental, and assignment
pub fn segregate_rels(rels: &[JboRelClause]) -> (Vec<&JboPred>, Vec<&JboPred>, Vec<&Result<SumtiAtom, JboTerm>>) {
    let mut restrictives = Vec::new();
    let mut incidentals = Vec::new();
    let mut assignments = Vec::new();

    for rel in rels {
        match rel {
            JboRelClause::JRRestrictive(pred) => restrictives.push(pred),
            JboRelClause::JRIncidental(pred) => incidentals.push(pred),
            JboRelClause::JRAssign(assign) => assignments.push(assign),
        }
    }

    (restrictives, incidentals, assignments)
}

// Rust integration: Preserve syntax-level connectives before semantic tag parsing is available at the call site.
// Semantic tag parsing uses `parse_connective_helper`, which ports JboParse.hs :: parseConnective with ParseM state.
pub fn parse_connective(conn: &Connective) -> Connective {
    conn.clone()
}

// Ported from: JboParse.hs :: mapRelsInBridi
/// Map a function over all relations in a bridi
pub fn map_rels_in_bridi<F>(f: F, bridi: crate::parse_m::Bridi) -> crate::parse_m::Bridi
where
    F: Fn(JboRel) -> JboRel + 'static,
{
    let f = std::rc::Rc::new(f);
    Box::new(move |args: &crate::parse_m::Args| {
        let prop = bridi(args);
        let f = f.clone();
        crate::logic::terp_prop(
            move |r: &JboRel, ts: &[JboTerm]| Prop::Rel(f(r.clone()), ts.to_vec()),
            |o: &JboModalOp| o.clone(),
            |q: &JboQuantifier| q.clone(),
            &prop,
        )
    })
}

/// Result of semantic analysis
#[derive(Debug, Clone)]
pub struct SemanticResult {
    /// The main proposition
    pub prop: JboProp,
    /// Veridical side propositions (for "lo" gadri)
    pub veridical: Vec<JboProp>,
    /// Non-veridical propositions (for "le" gadri)
    pub non_veridical: Vec<JboProp>,
    /// Side propositions in Haskell sideTexticules order.
    pub side_props: Vec<JboProp>,
    pub side_texticules: Vec<Texticule>,
    /// Lojban output with variables
    pub lojban_output: String,
}

// Ported from: JboParse.hs :: evalText
/// Parse a Text into semantic representation
pub fn eval_text(text: &Text) -> Vec<SemanticResult> {
    // Use the full parser with quantification support
    eval_text_with_full_parser(text)
}

// Rust adaptation: Full parser version with quantification support
fn eval_text_with_full_parser(text: &Text) -> Vec<SemanticResult> {
    let mut state = ParseState::new();
    let mut results = vec![];
    let all_items: Vec<&FragmentOrStatement> = text.text_paras.iter().flat_map(|p| p.iter()).collect();

    for (i, item) in all_items.iter().enumerate() {
        match item {
            FragmentOrStatement::Statement(stmt) => {
                let merged_stmt = if i == 0 && !text.text_frees.is_empty() {
                    let mut merged_frees = text.text_frees.clone();
                    merged_frees.extend(stmt.frees.clone());
                    Statement {
                        frees: merged_frees,
                        prenex: stmt.prenex.clone(),
                        body: stmt.body.clone(),
                    }
                } else {
                    stmt.clone()
                };

                if let Some(result) = eval_statement_full(&mut state, &merged_stmt) {
                    results.push(result);
                }
            }
            FragmentOrStatement::Fragment(frag) => {
                if i == 0 && !text.text_frees.is_empty() {
                    let _ = do_frees(&text.text_frees, &mut state);
                }
                if let Some(result) = eval_fragment_full(&mut state, frag) {
                    results.push(result);
                }
            }
        }
    }

    let side_texticules = state.take_side_texticules();
    let side_props = side_texticules_to_props(side_texticules.clone());
    let side_texticules = side_texticules.into_iter().map(prop_texticule_to_jbo).collect::<Vec<_>>();
    if !side_props.is_empty() {
        if let Some(first) = results.first_mut() {
            first.side_props = side_props;
            first.side_texticules = side_texticules;
            first.veridical.clear();
            first.non_veridical.clear();
            split_side_props(&first.side_props, &mut first.veridical, &mut first.non_veridical);
            first.lojban_output = match &first.prop {
                Prop::Rel(JboRel::Brivla(name), terms) if name == "fragment" => {
                    crate::jbo_show::build_lojban_output_from_fragment(terms, &first.side_props)
                }
                _ => build_lojban_output_from_texticules(&first.prop, &first.side_texticules),
            };
        }
    }

    results
}

// Ported from: JboParse.hs :: evalText (texticule collection path)
fn eval_texticules(text: &Text) -> Vec<Texticule> {
    let mut state = ParseState::new();
    let mut result = Vec::new();

    // Haskell: evalText (Text fs nai paras) = do
    //     jt <- mapM evalFragOrStatement $ zip (concat paras) $ fs:repeat []
    // First item gets text_frees, rest get empty lists
    let all_items: Vec<&FragmentOrStatement> = text.text_paras.iter().flat_map(|p| p.iter()).collect();

    for (i, item) in all_items.iter().enumerate() {
        let item_frees = if i == 0 { &text.text_frees } else { &Vec::new() };

        match item {
            FragmentOrStatement::Statement(stmt) => {
                // Merge text-level frees into statement frees for first item
                let merged_stmt = if i == 0 && !item_frees.is_empty() {
                    let mut merged_frees = item_frees.clone();
                    merged_frees.extend(stmt.frees.clone());
                    Statement {
                        frees: merged_frees,
                        prenex: stmt.prenex.clone(),
                        body: stmt.body.clone(),
                    }
                } else {
                    stmt.clone()
                };

                if let Some(semantic) = eval_statement_full(&mut state, &merged_stmt) {
                    result.push(Texticule::TexticuleProp(semantic.prop));
                }
            }
            FragmentOrStatement::Fragment(frag) => {
                // Process frees for fragments
                if i == 0 && !item_frees.is_empty() {
                    let _ = do_frees(item_frees, &mut state);
                }

                match frag {
                    Fragment::FragTerms(terms) => {
                        let mut bridi_state = crate::parse_m::BridiParseState::new();
                        state.non_veridical_props.clear();
                        if let Ok(parsed_terms) = parse_terms(terms, &mut bridi_state, &mut state) {
                            result.push(Texticule::TexticuleFrag(JboFragment::JboFragTerms(parsed_terms)));
                        } else {
                            result.push(Texticule::TexticuleFrag(JboFragment::JboFragUnparsed(frag.clone())));
                        }
                    }
                    _ => result.push(Texticule::TexticuleFrag(JboFragment::JboFragUnparsed(frag.clone()))),
                }
            }
        }
    }

    let side_texticules = state.take_side_texticules();
    let mut with_sides: Vec<Texticule> = side_texticules.into_iter().map(prop_texticule_to_jbo).collect();
    with_sides.extend(result);
    with_sides
}

// Rust adaptation: Full parser version of eval_statement
fn eval_statement_full(state: &mut ParseState, stmt: &Statement) -> Option<SemanticResult> {
    state.non_veridical_props.clear();
    let prop = parse_statement(stmt, state).ok()?;
    let discursives = statement_frees(stmt)
        .into_iter()
        .filter(is_discursive_free)
        .collect::<Vec<_>>();
    let prop = if discursives.is_empty() {
        prop
    } else {
        let sides = process_discursive_frees(&discursives, state).ok()?;
        if sides.is_empty() {
            prop
        } else {
            let sides_for_term = sides.clone();
            let f = std::rc::Rc::new(move |term| JboTerm::TermWithSides(Box::new(term), sides_for_term.clone()));
            let (prop, changed) = replace_last_term_in_prop_with_status(f, prop);
            if !changed {
                for side in sides {
                    state.add_side_texticule(prop_texticule_to_parse(side).ok()?);
                }
            }
            prop
        }
    };
    let prop = crate::parse_m::do_questions(true, state, prop);
    let prop = state.apply_transforms(prop);
    let side_props = vec![];
    let veridical = Vec::new();
    let non_veridical = Vec::new();
    let lojban_output = build_lojban_output_from_prop(&prop, &side_props);

    Some(SemanticResult {
        prop,
        veridical,
        non_veridical,
        side_props,
        side_texticules: Vec::new(),
        lojban_output,
    })
}

// Rust adaptation: Full parser version of eval_sentence
fn eval_sentence_full(state: &mut ParseState, sentence: &Sentence) -> Option<SemanticResult> {
    // Clear non-veridical props from previous sentence
    state.non_veridical_props.clear();

    // Create BridiParseState for this sentence
    // Ported from: JboParse.hs :: parseSentence (line 151)
    //   parseSentence (Sentence ts bt) = parseTerms ts >> parseBTail bt
    let mut bridi_state = crate::parse_m::BridiParseState::new();

    // Parse sentence-level terms - this adds them to bridi_state.arglist via add_arg
    let _all_terms = parse_terms(&sentence.terms, &mut bridi_state, state).ok()?;

    // Parse bridi tail to get the bridi function
    let bridi = parse_bridi_tail(&sentence.tail, &mut bridi_state, state).ok()?;

    // Apply pending transforms
    let bridi = crate::parse_m::apply_transforms_to_bridi(bridi, state);

    // Use args from bridi_state.arglist instead of manually building them
    // This ensures we get all terms including those from tail
    let prop = bridi(&bridi_state.arglist.args);

    let side_props = vec![];
    let veridical = Vec::new();
    let non_veridical = Vec::new();

    let lojban_output = build_lojban_output_from_prop(&prop, &side_props);

    Some(SemanticResult {
        prop,
        veridical,
        non_veridical,
        side_props,
        side_texticules: Vec::new(),
        lojban_output,
    })
}

// Rust-only: Convert parse_m::Texticule to JboProp
fn prop_texticule_to_prop(texticule: crate::parse_m::Texticule) -> JboProp {
    match texticule {
        crate::parse_m::Texticule::TexticuleProp(prop) => prop,
        crate::parse_m::Texticule::TexticuleSide(_, inner) => prop_texticule_to_prop(*inner),
    }
}

// Rust-only: Convert parse_m::Texticule to jbo_prop::Texticule
fn prop_texticule_to_jbo(texticule: crate::parse_m::Texticule) -> Texticule {
    match texticule {
        crate::parse_m::Texticule::TexticuleProp(prop) => Texticule::TexticuleProp(prop),
        crate::parse_m::Texticule::TexticuleSide(side_type, inner) => {
            Texticule::TexticuleSide(side_type, Box::new(prop_texticule_to_jbo(*inner)))
        }
    }
}

// Rust-only: Convert jbo_prop::Texticule to parse_m::Texticule
fn prop_texticule_to_parse(texticule: Texticule) -> Result<crate::parse_m::Texticule, String> {
    match texticule {
        Texticule::TexticuleProp(prop) => Ok(crate::parse_m::Texticule::TexticuleProp(prop)),
        Texticule::TexticuleSide(side_type, inner) => Ok(crate::parse_m::Texticule::TexticuleSide(
            side_type,
            Box::new(prop_texticule_to_parse(*inner)?),
        )),
        Texticule::TexticuleFrag(_) => Err("Unexpected fragment side texticule".to_string()),
    }
}

// Rust-only: Convert side texticules to propositions
fn side_texticules_to_props(side_texticules: Vec<crate::parse_m::Texticule>) -> Vec<JboProp> {
    side_texticules
        .into_iter()
        .map(prop_texticule_to_prop)
        .collect()
}

// Rust-only: Split side propositions into veridical and non-veridical
fn split_side_props(side_props: &[JboProp], veridical: &mut Vec<JboProp>, non_veridical: &mut Vec<JboProp>) {
    for prop in side_props {
        match prop {
            Prop::Modal(JboModalOp::NonVeridical, inner) => non_veridical.push(inner.as_ref().clone()),
            _ => extract_veridical_props(prop, veridical),
        }
    }
}

// Rust-only: Extract veridical propositions (non-NonVeridical)
fn extract_veridical_props(prop: &JboProp, result: &mut Vec<JboProp>) {
    match prop {
        Prop::Modal(JboModalOp::NonVeridical, _) => {}
        Prop::Rel(_, _) => result.push(prop.clone()),
        Prop::Connected(_, p1, p2) => {
            extract_veridical_props(p1, result);
            extract_veridical_props(p2, result);
        }
        Prop::Quantified(_, _, _) => {}
        _ => {}
    }
}

// Rust-only: Extract non-veridical (rel, term) pairs
fn extract_non_veridical_pairs(prop: &JboProp, result: &mut Vec<(JboRel, JboTerm)>) {
    match prop {
        Prop::Modal(JboModalOp::NonVeridical, inner) => {
            extract_rel_term_pairs(inner, result);
        }
        Prop::Connected(_, p1, p2) => {
            extract_non_veridical_pairs(p1, result);
            extract_non_veridical_pairs(p2, result);
        }
        Prop::Rel(_, _) => {}
        _ => {}
    }
}

// Rust-only: Extract (rel, term) pairs from a proposition
fn extract_rel_term_pairs(prop: &JboProp, result: &mut Vec<(JboRel, JboTerm)>) {
    match prop {
        Prop::Rel(rel, terms) => {
            if let Some(term) = terms.first() {
                result.push((rel.clone(), term.clone()));
            }
        }
        Prop::Connected(_, p1, p2) => {
            extract_rel_term_pairs(p1, result);
            extract_rel_term_pairs(p2, result);
        }
        Prop::Quantified(_, _, _) => {}
        _ => {}
    }
}

// Rust adaptation: Full parser version of eval_fragment
fn eval_fragment_full(state: &mut ParseState, frag: &Fragment) -> Option<SemanticResult> {
    // Ported from: JboParse.hs :: parseFrag (lines 62-64)
    // parseFrag :: Fragment -> ParseStateM JboFragment
    // parseFrag (FragTerms ts) = evalParseM $ JboFragTerms <$> parseTerms ts
    // parseFrag f = return $ JboFragUnparsed f

    match frag {
        Fragment::FragTerms(terms) => {
            // Create BridiParseState for fragment parsing
            let mut bridi_state = crate::parse_m::BridiParseState::new();

            // Save arglist before parsing (ignoringArgs pattern from ParseM.hs)
            let saved_arglist = bridi_state.arglist.clone();
            state.non_veridical_props.clear();

            // Parse terms using the full parsing machinery
            let result = match parse_terms(terms, &mut bridi_state, state) {
                Ok(parsed_terms) if !parsed_terms.is_empty() => {
                    let lojban_output = crate::jbo_show::build_lojban_output_from_fragment(
                        &parsed_terms,
                        &[]
                    );

                    Some(SemanticResult {
                        prop: Prop::Rel(JboRel::Brivla("fragment".to_string()), parsed_terms.clone()),
                        veridical: vec![],
                        non_veridical: vec![],
                        side_props: vec![],
                        side_texticules: Vec::new(),
                        lojban_output,
                    })
                }
                Ok(_) => Some(SemanticResult {
                    prop: Prop::Rel(JboRel::Brivla("fragment".to_string()), vec![]),
                    veridical: vec![],
                    non_veridical: vec![],
                    side_props: vec![],
                    side_texticules: Vec::new(),
                    lojban_output: String::new(),
                }),
                Err(_) => Some(unparsed_fragment_result(frag)),
            };

            // Restore arglist (ignoringArgs pattern)
            bridi_state.arglist = saved_arglist;

            result
        }
        _ => Some(unparsed_fragment_result(frag)),
    }
}

// Rust-only: Create semantic result for unparsed fragments
fn unparsed_fragment_result(frag: &Fragment) -> SemanticResult {
    let lojban_output = match frag {
        Fragment::FragLaName(name) => name.clone(),
        Fragment::FragCon(_)
        | Fragment::FragPrenex(_)
        | Fragment::FragQuantifier(_)
        | Fragment::FragNA(_)
        | Fragment::FragRels(_)
        | Fragment::FragLinks(_)
        | Fragment::FragTerms(_) => "li'o".to_string(),
    };

    SemanticResult {
        prop: Prop::Rel(JboRel::Brivla("unparsed-fragment".to_string()), vec![]),
        veridical: vec![],
        non_veridical: vec![],
        side_props: vec![],
        side_texticules: Vec::new(),
        lojban_output,
    }
}

// ============================================================================
// Mex and Operator parsing
// ============================================================================

// Ported from: JboParse.hs :: nullOp
/// Null operator (ge'a)
pub fn null_op() -> Operator {
    Operator::OpVUhU("ge'a".to_string())
}

// Ported from: JboParse.hs :: stripNulls
/// Strip null mex values and flatten null operators
fn strip_nulls(ms: Vec<JboMex>) -> Vec<JboMex> {
    let mut result = Vec::new();
    for m in ms {
        match &m {
            JboMex::Operation(op, os) if is_null_op(op) => {
                result.extend(strip_nulls(os.clone()));
            }
            _ if is_null_mex(&m) => {
                // Skip null mex
            }
            _ => {
                result.push(m);
            }
        }
    }
    result
}

// Ported from: JboParse.hs :: stripNulls (helper for null mex detection)
fn is_null_mex(m: &JboMex) -> bool {
    matches!(m, JboMex::MexNumeralString(ns) if ns.len() == 1 && matches!(&ns[0], Numeral::PA(s) if s == "tu'o"))
}

// Ported from: JboParse.hs :: stripNulls (helper for null operator detection)
fn is_null_op(op: &JboOperator) -> bool {
    matches!(op, JboOperator::OpVUhU(s) if s == "ge'a")
}

// Ported from: JboParse.hs :: applyJboOperator
/// Apply operator to operands, handling permutation and null operators
fn apply_jbo_operator(op: JboOperator, os: Vec<JboMex>) -> JboMex {
    match op {
        JboOperator::OpPermuted(s, inner_op) => {
            let stripped = strip_nulls(os);
            let swapped = crate::util::swap_finite_with_default(
                null_jbo_mex(),
                &stripped,
                0,
                (s - 1) as usize,
            );
            apply_jbo_operator(*inner_op, swapped)
        }
        op if is_null_op(&op) => {
            // Flatten: if first operand is an operation, merge its operands
            if let Some(JboMex::Operation(inner_op, inner_os)) = os.first() {
                let mut merged = inner_os.clone();
                // Clone os to avoid move-after-borrow
                let os_clone = os.clone();
                merged.extend(os_clone.into_iter().skip(1));
                apply_jbo_operator((**inner_op).clone(), merged)
            } else {
                JboMex::Operation(Box::new(op), strip_nulls(os))
            }
        }
        _ => JboMex::Operation(Box::new(op), strip_nulls(os)),
    }
}

// Rust-only: Helper for null mex construction
fn null_jbo_mex() -> JboMex {
    JboMex::MexNumeralString(vec![Numeral::PA("tu'o".to_string())])
}

// Ported from: JboParse.hs :: parseMex (logical connective branch extraction)
fn logical_connected_mex_branches(m: &JboMex) -> Option<(crate::jbo_syntax::LogJboConnective, JboMex, JboMex)> {
    match m {
        JboMex::ConnectedMex(_, Connective::JboConnLog(_, lcon), m1, m2) => {
            Some((lcon.clone(), (**m1).clone(), (**m2).clone()))
        }
        JboMex::Operation(op, args) => match op.as_ref() {
            JboOperator::ConnectedOperator(_, Connective::JboConnLog(_, lcon), op1, op2) => Some((
                lcon.clone(),
                apply_jbo_operator((**op1).clone(), args.clone()),
                apply_jbo_operator((**op2).clone(), args.clone()),
            )),
            _ => args.iter().enumerate().find_map(|(i, arg)| {
                let (lcon, left_arg, right_arg) = logical_connected_mex_branches(arg)?;
                let mut left_args = args.clone();
                left_args[i] = left_arg;
                let mut right_args = args.clone();
                right_args[i] = right_arg;
                Some((
                    lcon,
                    apply_jbo_operator((**op).clone(), left_args),
                    apply_jbo_operator((**op).clone(), right_args),
                ))
            }),
        },
        _ => None,
    }
}

// Ported from: JboParse.hs :: parseMex
/// Parse a mex expression, reducing operations
pub fn parse_mex(m: &Mex, state: &mut ParseState) -> Result<JboMex, String> {
    let parsed = parse_mex_inner(m, state)?;
    Ok(reduce_mex(parsed))
}

// Ported from: JboParse.hs :: parseMex (reduction helper)
fn reduce_mex(m: JboMex) -> JboMex {
    match m {
        JboMex::Operation(op, ms) => apply_jbo_operator(*op, ms),
        _ => m,
    }
}

// Ported from: JboParse.hs :: _parseMex
/// Internal mex parser
fn parse_mex_inner(m: &Mex, state: &mut ParseState) -> Result<JboMex, String> {
    match m {
        Mex::ConnectedMex(fore, con @ AbsConnective::JboConnLog(_, _), m1, m2) => {
            do_connective(
                *fore,
                con,
                |state| parse_mex_inner(m1, state),
                |state| parse_mex_inner(m2, state),
                state,
            )
        }
        Mex::ConnectedMex(fore, con @ AbsConnective::JboConnJoik(_, _), m1, m2) => {
            let parsed_con = parse_connective(con);
            let parsed_m1 = parse_mex_inner(m1, state)?;
            let parsed_m2 = parse_mex_inner(m2, state)?;
            Ok(JboMex::ConnectedMex(*fore, parsed_con, Box::new(parsed_m1), Box::new(parsed_m2)))
        }
        Mex::Operation(op, ms) => {
            let parsed_op = parse_operator(op, state)?;
            let parsed_ms: Result<Vec<_>, _> = ms.iter()
                .map(|m| parse_mex_inner(m, state))
                .collect();
            Ok(JboMex::Operation(Box::new(parsed_op), parsed_ms?))
        }
        Mex::MexArray(ms) => {
            let parsed_ms: Result<Vec<_>, _> = ms.iter()
                .map(|m| parse_mex_inner(m, state))
                .collect();
            Ok(JboMex::MexArray(parsed_ms?))
        }
        Mex::QualifiedMex(q, m) => {
            let parsed_m = parse_mex_inner(m, state)?;
            Ok(JboMex::QualifiedMex(q.clone(), Box::new(parsed_m)))
        }
        Mex::MexSelbri(sb) => {
            let vpred = selbri_to_vpred(sb, state)?;
            Ok(JboMex::MexSelbri(Box::new(vpred)))
        }
        Mex::MexSumti(s) => {
            let parsed_s = parse_sumti(s, state)?;
            Ok(JboMex::MexSumti(Box::new(parsed_s)))
        }
        Mex::MexInt(n) => Ok(JboMex::MexInt(*n)),
        Mex::MexNumeralString(ns) => Ok(JboMex::MexNumeralString(ns.clone())),
        Mex::MexLerfuString(ls) => Ok(JboMex::MexLerfuString(ls.clone())),
    }
}

// Ported from: JboParse.hs :: parseOperator
/// Parse an operator expression
pub fn parse_operator(op: &Operator, state: &mut ParseState) -> Result<JboOperator, String> {
    match op {
        Operator::ConnectedOperator(fore, con @ AbsConnective::JboConnLog(_, _), o1, o2) => {
            do_connective(
                *fore,
                con,
                |state| parse_operator(o1, state),
                |state| parse_operator(o2, state),
                state,
            )
        }
        Operator::ConnectedOperator(fore, con @ AbsConnective::JboConnJoik(_, _), o1, o2) => {
            let parsed_con = parse_connective(con);
            let parsed_o1 = parse_operator(o1, state)?;
            let parsed_o2 = parse_operator(o2, state)?;
            Ok(JboOperator::ConnectedOperator(*fore, parsed_con, Box::new(parsed_o1), Box::new(parsed_o2)))
        }
        Operator::OpPermuted(s, o) => {
            let parsed_o = parse_operator(o, state)?;
            Ok(JboOperator::OpPermuted(*s, Box::new(parsed_o)))
        }
        Operator::OpScalarNegated(n, o) => {
            let parsed_o = parse_operator(o, state)?;
            Ok(JboOperator::OpScalarNegated(n.clone(), Box::new(parsed_o)))
        }
        Operator::OpMex(m) => {
            let parsed_m = parse_mex_inner(m, state)?;
            Ok(JboOperator::OpMex(Box::new(parsed_m)))
        }
        Operator::OpSelbri(sb) => {
            let vpred = selbri_to_vpred(sb, state)?;
            Ok(JboOperator::OpSelbri(Box::new(vpred)))
        }
        Operator::OpVUhU(v) => Ok(JboOperator::OpVUhU(v.clone())),
    }
}

// ============================================================================
// Stub implementations for functions called by parseMex/parseOperator
// ============================================================================

// Ported from: JboParse.hs :: doConnective
/// Handle connectives with forethought/afterthought logic
///
/// Original Haskell:
///   doConnective :: PreProp r => Bool -> Connective -> ParseM r a -> ParseM r a -> ParseM r a
///   doConnective isForethought con m1 m2 = do
///       let (mtag,con') = case con of
///               JboConnLog mtag lcon -> (mtag, connToFOL lcon)
///               JboConnJoik mtag joik -> (mtag, joikToFOL joik)
///       (m1',m2') <- case mtag of
///           Nothing -> return $ (m1,m2)
///           Just tag -> do
///               v <- quantify mexExists Nothing
///               if isForethought then do
///                       tag' <- parseTag tag
///                       return $ (doModal (WithEventAs v) >> m1
///                           , (doTag tag' $ Just v) >> m2)
///                   else if isTense tag
///                       then return $ (doModal (WithEventAs v) >> m1
///                               , parseTag tag >>= (`doTag` Just v) >> m2)
///                       else return $ ( (parseTag tag >>= (`doTag` Just v)) >> m1
///                               , doModal (WithEventAs v) >> m2)
///       mapParseM2 con' m1' m2'
fn do_connective_prop<F1, F2>(
    is_forethought: bool,
    con: &Connective,
    f1: F1,
    f2: F2,
    state: &mut ParseState,
) -> Result<JboProp, String>
where
    F1: FnOnce(&mut ParseState) -> Result<JboProp, String>,
    F2: FnOnce(&mut ParseState) -> Result<JboProp, String>,
{
    let mut outer_transforms = std::mem::take(&mut state.pending_prop_transforms);
    let mut left_state = state.clone();
    let mut right_state = state.clone();

    let mtag = match con {
        Connective::JboConnLog(mtag, _) => mtag.as_ref(),
        Connective::JboConnJoik(mtag, _) => mtag.as_ref(),
    };
    let outer_transform_start = state.pending_prop_transforms.len();

    if let Some(tag) = mtag {
        let v = quantify_helper(
            &crate::jbo_syntax::Mex::MexNumeralString(vec![
                crate::jbo_syntax::Numeral::PA("su'o".to_string())
            ]),
            None,
            state,
        )?;
        outer_transforms.extend(state.pending_prop_transforms.split_off(outer_transform_start));

        left_state.next_fresh_var = state.next_fresh_var;
        left_state.variable_domains = state.variable_domains.clone();
        right_state.next_fresh_var = state.next_fresh_var;
        right_state.variable_domains = state.variable_domains.clone();

        if is_forethought {
            let jtag = parse_tag_helper(tag, state)?;
            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), &mut left_state);
            do_tag_helper(&jtag, Some(v), &mut right_state)?;
        } else if is_tense_tag(tag) {
            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), &mut left_state);
            let jtag = parse_tag_helper(tag, state)?;
            do_tag_helper(&jtag, Some(v), &mut right_state)?;
        } else {
            let jtag = parse_tag_helper(tag, state)?;
            do_tag_helper(&jtag, Some(v.clone()), &mut left_state)?;
            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v), &mut right_state);
        }
    }

    let left_prop = f1(&mut left_state)?;
    let left_transforms = left_state.pending_prop_transforms.drain(..).collect::<Vec<_>>();
    right_state.sumbasti_bindings = left_state.sumbasti_bindings.clone();
    right_state.backcount_stack = left_state.backcount_stack.clone();
    right_state.bribasti_bindings = left_state.bribasti_bindings.clone();
    right_state.next_fresh_var = left_state.next_fresh_var;
    right_state.next_fresh_rvar = left_state.next_fresh_rvar;
    right_state.variable_domains = left_state.variable_domains.clone();
    right_state.next_fresh_constant = left_state.next_fresh_constant;
    right_state.referenced_vars = left_state.referenced_vars.clone();
    right_state.questions = left_state.questions.clone();
    right_state.lambdas = left_state.lambdas.clone();
    right_state.side_texticules = left_state.side_texticules.clone();
    right_state.non_veridical_props = left_state.non_veridical_props.clone();

    let right_prop = f2(&mut right_state)?;
    let right_transforms = right_state.pending_prop_transforms.drain(..).collect::<Vec<_>>();
    let left_prop = apply_transform_list(left_transforms, left_prop);
    let right_prop = apply_transform_list(right_transforms, right_prop);

    *state = right_state;

    let prop = match con {
        Connective::JboConnLog(_, lcon) => crate::jbo_prop::conn_to_fol(lcon, left_prop, right_prop),
        Connective::JboConnJoik(_, joik) => crate::jbo_prop::joik_to_fol(joik.clone(), left_prop, right_prop),
    };
    Ok(apply_transform_list(outer_transforms, prop))
}

trait ConnectiveResult: Sized {
    fn combine_connective(is_forethought: bool, con: Connective, left: Self, right: Self) -> Self;
}

impl ConnectiveResult for JboMex {
    fn combine_connective(is_forethought: bool, con: Connective, left: Self, right: Self) -> Self {
        JboMex::ConnectedMex(is_forethought, con, Box::new(left), Box::new(right))
    }
}

impl ConnectiveResult for JboOperator {
    fn combine_connective(is_forethought: bool, con: Connective, left: Self, right: Self) -> Self {
        JboOperator::ConnectedOperator(is_forethought, con, Box::new(left), Box::new(right))
    }
}

// Ported from: JboParse.hs :: doConnective (generic version for ConnectiveResult types)
fn do_connective<T, F1, F2>(
    is_forethought: bool,
    con: &Connective,
    f1: F1,
    f2: F2,
    state: &mut ParseState,
) -> Result<T, String>
where
    T: ConnectiveResult,
    F1: FnOnce(&mut ParseState) -> Result<T, String>,
    F2: FnOnce(&mut ParseState) -> Result<T, String>,
{
    let mtag = match con {
        Connective::JboConnLog(mtag, _) => mtag.as_ref(),
        Connective::JboConnJoik(mtag, _) => mtag.as_ref(),
    };
    let outer_transform_start = state.pending_prop_transforms.len();
    let mut outer_transforms = Vec::new();
    let mut left_state = state.clone();
    let mut right_state = state.clone();

    if let Some(tag) = mtag {
        let v = quantify_helper(
            &crate::jbo_syntax::Mex::MexNumeralString(vec![
                crate::jbo_syntax::Numeral::PA("su'o".to_string())
            ]),
            None,
            state,
        )?;
        outer_transforms.extend(state.pending_prop_transforms.split_off(outer_transform_start));

        left_state.next_fresh_var = state.next_fresh_var;
        left_state.variable_domains = state.variable_domains.clone();
        right_state.next_fresh_var = state.next_fresh_var;
        right_state.variable_domains = state.variable_domains.clone();

        if is_forethought {
            let jtag = parse_tag_helper(tag, state)?;
            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), &mut left_state);
            do_tag_helper(&jtag, Some(v), &mut right_state)?;
        } else if is_tense_tag(tag) {
            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), &mut left_state);
            let jtag = parse_tag_helper(tag, state)?;
            do_tag_helper(&jtag, Some(v), &mut right_state)?;
        } else {
            let jtag = parse_tag_helper(tag, state)?;
            do_tag_helper(&jtag, Some(v.clone()), &mut left_state)?;
            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v), &mut right_state);
        }
    }

    let left = f1(&mut left_state)?;
    right_state.sumbasti_bindings = left_state.sumbasti_bindings.clone();
    right_state.backcount_stack = left_state.backcount_stack.clone();
    right_state.bribasti_bindings = left_state.bribasti_bindings.clone();
    right_state.next_fresh_var = left_state.next_fresh_var;
    right_state.next_fresh_rvar = left_state.next_fresh_rvar;
    right_state.variable_domains = left_state.variable_domains.clone();
    right_state.next_fresh_constant = left_state.next_fresh_constant;
    right_state.referenced_vars = left_state.referenced_vars.clone();
    right_state.questions = left_state.questions.clone();
    right_state.lambdas = left_state.lambdas.clone();
    right_state.side_texticules = left_state.side_texticules.clone();
    right_state.non_veridical_props = left_state.non_veridical_props.clone();

    let right = f2(&mut right_state)?;
    *state = right_state;
    state.pending_prop_transforms.splice(0..0, outer_transforms);
    Ok(T::combine_connective(is_forethought, parse_connective(con), left, right))
}

// Ported from: JboParse.hs :: doConnective (bridi version with BridiParseState)
fn do_connective_with_bridi<F1, F2>(
    is_forethought: bool,
    con: &Connective,
    f1: F1,
    f2: F2,
    bridi_state: &mut crate::parse_m::BridiParseState,
    state: &mut ParseState,
) -> Result<Bridi, String>
where
    F1: FnOnce(&mut crate::parse_m::BridiParseState, &mut ParseState) -> Result<Bridi, String>,
    F2: FnOnce(&mut crate::parse_m::BridiParseState, &mut ParseState) -> Result<Bridi, String>,
{
    let mut outer_transforms = std::mem::take(&mut state.pending_prop_transforms);
    let mut left_bridi_state = bridi_state.clone();
    let mut left_state = state.clone();
    let mut right_bridi_state = bridi_state.clone();
    let mut right_state = state.clone();

    let mtag = match con {
        Connective::JboConnLog(mtag, _) => mtag.as_ref(),
        Connective::JboConnJoik(mtag, _) => mtag.as_ref(),
    };
    let outer_transform_start = state.pending_prop_transforms.len();

    if let Some(tag) = mtag {
        let v = quantify_helper(
            &crate::jbo_syntax::Mex::MexNumeralString(vec![
                crate::jbo_syntax::Numeral::PA("su'o".to_string())
            ]),
            None,
            state,
        )?;
        outer_transforms.extend(state.pending_prop_transforms.split_off(outer_transform_start));

        left_state.next_fresh_var = state.next_fresh_var;
        left_state.variable_domains = state.variable_domains.clone();
        right_state.next_fresh_var = state.next_fresh_var;
        right_state.variable_domains = state.variable_domains.clone();

        if is_forethought {
            let jtag = parse_tag_helper(tag, state)?;
            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), &mut left_state);
            do_tag_helper(&jtag, Some(v), &mut right_state)?;
        } else if is_tense_tag(tag) {
            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), &mut left_state);
            let jtag = parse_tag_helper(tag, state)?;
            do_tag_helper(&jtag, Some(v), &mut right_state)?;
        } else {
            let jtag = parse_tag_helper(tag, state)?;
            do_tag_helper(&jtag, Some(v.clone()), &mut left_state)?;
            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v), &mut right_state);
        }
    }

    let left_bridi = f1(&mut left_bridi_state, &mut left_state)?;
    let left_args = left_bridi_state.arglist.args.clone();
    right_state.sumbasti_bindings = left_state.sumbasti_bindings.clone();
    right_state.backcount_stack = left_state.backcount_stack.clone();
    right_state.bribasti_bindings = left_state.bribasti_bindings.clone();
    right_state.next_fresh_var = left_state.next_fresh_var;
    right_state.next_fresh_rvar = left_state.next_fresh_rvar;
    right_state.variable_domains = left_state.variable_domains.clone();
    right_state.next_fresh_constant = left_state.next_fresh_constant;
    right_state.referenced_vars = left_state.referenced_vars.clone();
    right_state.questions = left_state.questions.clone();
    right_state.lambdas = left_state.lambdas.clone();
    right_state.side_texticules = left_state.side_texticules.clone();
    right_state.non_veridical_props = left_state.non_veridical_props.clone();
    let right_bridi = f2(&mut right_bridi_state, &mut right_state)?;
    let right_args = right_bridi_state.arglist.args.clone();

    let left_transforms = left_state.pending_prop_transforms.drain(..).collect::<Vec<_>>();
    let right_transforms = right_state.pending_prop_transforms.drain(..).collect::<Vec<_>>();
    let left_bridi = apply_transform_list_to_bridi(left_bridi, left_transforms);
    let right_bridi = apply_transform_list_to_bridi(right_bridi, right_transforms);

    *bridi_state = right_bridi_state;
    *state = right_state;

    let con = con.clone();
    Ok(Box::new(move |args| {
        let left_prop = if is_forethought {
            let continuation_args = continuation_args_after_branch(args, &right_args);
            left_bridi(&crate::parse_m::join_args(&continuation_args, &left_args))
        } else {
            let continuation_args = continuation_args_after_branch(args, &right_args);
            left_bridi(&crate::parse_m::join_args(&continuation_args, &left_args))
        };
        let right_prop = if is_forethought {
            let continuation_args = continuation_args_after_branch(args, &right_args);
            right_bridi(&crate::parse_m::join_args(&continuation_args, &right_args))
        } else {
            let continuation_args = continuation_args_after_branch(args, &right_args);
            right_bridi(&crate::parse_m::join_args(&continuation_args, &right_args))
        };
        let prop = match &con {
            Connective::JboConnLog(_, lcon) => crate::jbo_prop::conn_to_fol(lcon, left_prop, right_prop),
            Connective::JboConnJoik(_, joik) => crate::jbo_prop::joik_to_fol(joik.clone(), left_prop, right_prop),
        };
        apply_transform_list(outer_transforms.clone(), prop)
    }))
}
// Ported from: JboParse.hs :: doConnective (continuation argument extraction)
fn continuation_args_after_branch(
    final_args: &crate::parse_m::Args,
    branch_args: &crate::parse_m::Args,
) -> crate::parse_m::Args {
    let mut used_branch_terms = branch_args.values().cloned().collect::<Vec<_>>();
    let mut continuation_terms = Vec::new();
    for (_, term) in sorted_args(final_args) {
        if let Some(idx) = used_branch_terms.iter().position(|branch_term| branch_term == term) {
            used_branch_terms.remove(idx);
        } else {
            continuation_terms.push(term.clone());
        }
    }
    continuation_terms
        .into_iter()
        .enumerate()
        .map(|(idx, term)| (crate::parse_m::ArgPos::UnfilledPos(idx as i32), term))
        .collect()
}

// Rust-only: Sort argument positions for deterministic ordering
fn sorted_args(args: &crate::parse_m::Args) -> Vec<(&crate::parse_m::ArgPos, &JboTerm)> {
    let mut items = args.iter().collect::<Vec<_>>();
    items.sort_by(|(a, _), (b, _)| a.cmp(b));
    items
}

// Helper: Apply FOL connective to two results (removed - not needed with current approach)

// Ported from: JboParse.hs :: parseTag (tense tag detection helper)
fn is_tense_tag(tag: &Tag) -> bool {
    match tag {
        Tag::DecoratedTagUnits(dtus) => {
            dtus.iter().any(|dtu| matches!(
                &dtu.tag_unit,
                TagUnit::TenseCmavo(_)
                | TagUnit::FAhA { .. }
                | TagUnit::TAhE_ZAhO { .. }
            ))
        }
        Tag::ConnectedTag(_, _, _) => false,
    }
}

// Ported from: JboParse.hs :: parseTag
fn parse_tag_helper(tag: &Tag, state: &mut ParseState) -> Result<crate::jbo_prop::JboTag, String> {
    match tag {
        Tag::ConnectedTag(con, tag1, tag2) => {
            let jcon = parse_connective_helper(con, state)?;
            let jtag1 = parse_tag_helper(tag1, state)?;
            let jtag2 = parse_tag_helper(tag2, state)?;
            Ok(crate::jbo_prop::JboTag::ConnectedTag(
                Box::new(jcon),
                Box::new(jtag1),
                Box::new(jtag2),
            ))
        }
        Tag::DecoratedTagUnits(dtus) => {
            let jdtus: Result<Vec<_>, String> = dtus
                .iter()
                .map(|dtu| {
                    let jtu = parse_tag_unit_helper(&dtu.tag_unit, state)?;
                    Ok(crate::jbo_prop::DecoratedTagUnit {
                        nahe: dtu.tag_nahe.clone(),
                        se: dtu.tag_se,
                        nai: dtu.tag_nai,
                        tag_unit: jtu,
                    })
                })
                .collect();
            Ok(crate::jbo_prop::JboTag::DecoratedTagUnits(jdtus?))
        }
    }
}

// Ported from: JboParse.hs :: parseTag (tag unit parsing)
fn parse_tag_unit_helper(
    tu: &TagUnit,
    state: &mut ParseState,
) -> Result<crate::jbo_prop::JboTagUnit, String> {
    match tu {
        TagUnit::TenseCmavo(c) => Ok(crate::jbo_prop::JboTagUnit::TenseCmavo(c.clone())),
        TagUnit::CAhA(c) => Ok(crate::jbo_prop::JboTagUnit::CAhA(c.clone())),
        TagUnit::BAI(c) => Ok(crate::jbo_prop::JboTagUnit::BAI(c.clone())),
        TagUnit::FAhA { faha_has_mohi, faha_cmavo } => {
            Ok(crate::jbo_prop::JboTagUnit::FAhA(Some(*faha_has_mohi), faha_cmavo.clone()))
        }
        TagUnit::TAhE_ZAhO { tahe_zohe_is_space, tahe_zaho_cmavo } => {
            Ok(crate::jbo_prop::JboTagUnit::TAhE_ZAhO(*tahe_zohe_is_space, tahe_zaho_cmavo.clone()))
        }
        TagUnit::ROI { roiroi, roi_is_space, roi_quantifier } => {
            let jq = parse_mex_helper(roi_quantifier, state)?;
            Ok(crate::jbo_prop::JboTagUnit::ROI(roiroi.clone(), *roi_is_space, jq))
        }
        TagUnit::FIhO(sb) => {
            let vpred = selbri_to_vpred(sb, state)?;
            Ok(crate::jbo_prop::JboTagUnit::FIhO(vpred))
        }
        TagUnit::KI => Ok(crate::jbo_prop::JboTagUnit::KI),
        TagUnit::CUhE(c) => Ok(crate::jbo_prop::JboTagUnit::CUhE(c.clone())),
    }
}

// Ported from: JboParse.hs :: parseConnective
fn parse_connective_helper(
    con: &Connective,
    state: &mut ParseState,
) -> Result<crate::jbo_prop::JboConnective, String> {
    match con {
        Connective::JboConnLog(mtag, lcon) => {
            let mtag_parsed = match mtag {
                Some(tag) => Some(parse_tag_helper(tag, state)?),
                None => None,
            };
            Ok(crate::jbo_prop::JboConnective::JboConnLog(
                mtag_parsed.map(Box::new),
                lcon.clone(),
            ))
        }
        Connective::JboConnJoik(mtag, joik) => {
            let mtag_parsed = match mtag {
                Some(tag) => Some(parse_tag_helper(tag, state)?),
                None => None,
            };
            Ok(crate::jbo_prop::JboConnective::JboConnJoik(
                mtag_parsed.map(Box::new),
                joik.clone(),
            ))
        }
    }
}

// Ported from: JboParse.hs :: doModal
fn do_modal_helper(op: &crate::jbo_prop::JboModalOp, state: &mut ParseState) {
    state.map_prop(crate::parse_m::PropTransform::ApplyModal(op.clone()));
}

// Ported from: JboParse.hs :: doTag (tag concatenation helper)
fn append_tags(left: crate::jbo_prop::JboTag, right: crate::jbo_prop::JboTag) -> crate::jbo_prop::JboTag {
    match (left, right) {
        (crate::jbo_prop::JboTag::DecoratedTagUnits(mut left), crate::jbo_prop::JboTag::DecoratedTagUnits(right)) => {
            left.extend(right);
            crate::jbo_prop::JboTag::DecoratedTagUnits(left)
        }
        (left, right) => crate::jbo_prop::JboTag::ConnectedTag(
            Box::new(crate::jbo_prop::JboConnective::JboConnJoik(None, "jo'u".to_string())),
            Box::new(left),
            Box::new(right),
        ),
    }
}

// Ported from: JboParse.hs :: doTag
fn do_tag_helper(
    jtag: &crate::jbo_prop::JboTag,
    mt: Option<JboTerm>,
    state: &mut ParseState,
) -> Result<(), String> {
    match jtag {
        crate::jbo_prop::JboTag::ConnectedTag(conn, tag1, tag2) if mt.is_none() => {
            match conn.as_ref() {
                crate::jbo_prop::JboConnective::JboConnLog(_, _) => {
                    let start = state.pending_prop_transforms.len();
                    do_tag_helper(tag1, None, state)?;
                    let left_transforms = state.pending_prop_transforms.split_off(start);
                    do_tag_helper(tag2, None, state)?;
                    let right_transforms = state.pending_prop_transforms.split_off(start);
                    state.map_prop(crate::parse_m::PropTransform::ConnectTagBranches {
                        conn: conn.as_ref().clone(),
                        left_transforms,
                        right_transforms,
                    });
                    Ok(())
                }
                crate::jbo_prop::JboConnective::JboConnJoik(_, _) => {
                    let modal = crate::jbo_prop::JboModalOp::Tagged(jtag.clone(), mt);
                    do_modal_helper(&modal, state);
                    Ok(())
                }
            }
        }
        crate::jbo_prop::JboTag::DecoratedTagUnits(dtus) if mt.is_some() => {
            let modal = crate::jbo_prop::JboModalOp::Tagged(jtag.clone(), mt);
            do_modal_helper(&modal, state);
            Ok(())
        }
        crate::jbo_prop::JboTag::DecoratedTagUnits(dtus) => {
            if dtus.is_empty() {
                return Ok(());
            }
            // Separate into stream of possibly negated modals
            for dtu in dtus {
                let scalar = tag_nai_is_scalar(&dtu.tag_unit);
                if dtu.nai && !scalar {
                    state.map_prop(crate::parse_m::PropTransform::Negate);
                }
                let modal = crate::jbo_prop::JboModalOp::Tagged(
                    crate::jbo_prop::JboTag::DecoratedTagUnits(vec![
                        crate::jbo_prop::DecoratedTagUnit {
                            nahe: dtu.nahe.clone(),
                            se: dtu.se,
                            nai: dtu.nai && scalar,
                            tag_unit: dtu.tag_unit.clone(),
                        },
                    ]),
                    None,
                );
                do_modal_helper(&modal, state);
            }
            Ok(())
        }
        _ => {
            // Apply tag with optional term
            let modal = crate::jbo_prop::JboModalOp::Tagged(jtag.clone(), mt);
            do_modal_helper(&modal, state);
            Ok(())
        }
    }
}

// Ported from: JboParse.hs :: doTag (NAI scalar detection for tag units)
fn tag_nai_is_scalar(tu: &crate::jbo_prop::JboTagUnit) -> bool {
    matches!(
        tu,
        crate::jbo_prop::JboTagUnit::ROI(_, _, _)
            | crate::jbo_prop::JboTagUnit::TAhE_ZAhO(_, _)
            | crate::jbo_prop::JboTagUnit::CAhA(_)
    )
}

// Ported from: JboParse.hs :: parseMex (wrapper)
fn parse_mex_helper(m: &crate::jbo_syntax::Mex, state: &mut ParseState) -> Result<JboMex, String> {
    parse_mex_inner(m, state)
}

// Ported from: JboParse.hs :: quantify (wrapper)
fn quantify_helper(
    m: &crate::jbo_syntax::Mex,
    r: Option<JboPred>,
    state: &mut ParseState,
) -> Result<JboTerm, String> {
    let jm = parse_mex_helper(m, state)?;
    quantify(&jm, r, state)
}

// Ported from: JboParse.hs :: doConnective (results version)
/// Handle connectives with already-computed results
fn do_connective_results<T: ConnectiveResult>(
    is_forethought: bool,
    con: &Connective,
    r1: T,
    r2: T,
    state: &mut ParseState,
) -> Result<T, String> {
    // Use the full doConnective implementation with closures that return the results
    do_connective(
        is_forethought,
        con,
        |_| Ok(r1),
        |_| Ok(r2),
        state,
    )
}

// Ported from: JboParse.hs :: selbriToVPred
/// Convert a selbri to a variadic predicate
fn selbri_to_vpred(sb: &Selbri, state: &mut ParseState) -> Result<crate::jbo_prop::JboVPred, String> {
    let mut bridi_state = crate::parse_m::BridiParseState::new();
    bridi_state.is_sub_bridi = true;
    let outer_transforms = std::mem::take(&mut state.pending_prop_transforms);
    let bridi_result = parse_selbri(sb, &mut bridi_state, state);
    let transforms = std::mem::take(&mut state.pending_prop_transforms);
    state.pending_prop_transforms = outer_transforms;
    let bridi = bridi_result?;
    let bridi = crate::parse_m::apply_transform_list_to_bridi(bridi, transforms);
    parsed_selbri_to_vpred(bridi, bridi_state.arglist.args)
}

// Ported from: JboParse.hs :: parsedSelbriToVPred
/// Convert a parsed bridi to a variadic predicate
fn parsed_selbri_to_vpred(bridi: Bridi, captured_args: crate::parse_m::Args) -> Result<crate::jbo_prop::JboVPred, String> {
    let resolved: Bridi = Box::new(move |extra_args: &crate::parse_m::Args| {
        let combined_args = crate::parse_m::join_args(extra_args, &captured_args);
        bridi(&combined_args)
    });
    Ok(bridi_to_jbo_vpred(resolved))
}

// Ported from: JboParse.hs :: selbriToSeltauRel
fn selbri_to_seltau_rel(sb: &Selbri, state: &mut ParseState) -> Result<JboRel, String> {
    let vpred = selbri_to_vpred(sb, state)?;
    match vpred(&[JboTerm::BoundVar(0)]) {
        Prop::Rel(rel, terms) => Ok(JboRel::AppliedRel(Box::new(rel), terms)),
        _ => Err("Selbri is not a simple relation".to_string()),
    }
}

// Ported from: JboParse.hs :: parseSelbri
/// Parse a selbri into a bridi
fn parse_selbri(sb: &Selbri, bridi_state: &mut crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Bridi, String> {
    Ok(match sb {
        Selbri::Negated(inner_sb) => {
            state.map_prop(crate::parse_m::PropTransform::Negate);
            parse_selbri(inner_sb, bridi_state, state)?
        }
        Selbri::TaggedSelbri(tag, inner_sb) => {
            let tag_parsed = parse_tag_helper(tag, state)?;
            do_bare_tag(&tag_parsed, state)?;
            parse_selbri(inner_sb, bridi_state, state)?
        }
        Selbri::Selbri2(sb2) => parse_selbri2(sb2, bridi_state, state)?,
    })
}

// Ported from: JboParse.hs :: parseSelbri2
/// Parse a Selbri2 into a bridi
fn parse_selbri2(sb2: &Selbri2, bridi_state: &mut crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Bridi, String> {
    match sb2 {
        Selbri2::SBInverted(sb3, sb2_inner) => {
            let seltau_bridi = parse_seltau_selbri2(sb2_inner, bridi_state, state)?;
            let tertau_bridi = parse_selbri3(sb3, bridi_state, state)?;
            apply_seltau(seltau_bridi, tertau_bridi)
        }
        Selbri2::Selbri3(sb3) => parse_selbri3(sb3, bridi_state, state),
    }
}

// Ported from: JboParse.hs :: parseSelbri2 (seltau parsing helper)
fn parse_seltau_selbri2(sb2: &Selbri2, bridi_state: &crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Bridi, String> {
    parse_seltau_bridi(bridi_state, state, |sub_state, state| parse_selbri2(sb2, sub_state, state))
}

// Ported from: JboParse.hs :: parseSelbri3 (seltau parsing helper)
fn parse_seltau_selbri3(sb3: &Selbri3, bridi_state: &crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Bridi, String> {
    parse_seltau_bridi(bridi_state, state, |sub_state, state| parse_selbri3(sb3, sub_state, state))
}

// Ported from: JboParse.hs :: parseSelbri (seltau parsing with isolated bridi state)
fn parse_seltau_bridi<F>(
    bridi_state: &crate::parse_m::BridiParseState,
    state: &mut ParseState,
    f: F,
) -> Result<Bridi, String>
where
    F: FnOnce(&mut crate::parse_m::BridiParseState, &mut ParseState) -> Result<Bridi, String>,
{
    let mut sub_state = bridi_state.clone();
    sub_state.arglist = crate::parse_m::Arglist::new();
    sub_state.is_sub_bridi = true;
    let transform_start = state.pending_prop_transforms.len();
    let bridi = f(&mut sub_state, state)?;
    let transforms = state.pending_prop_transforms.split_off(transform_start);
    let bridi = crate::parse_m::apply_transform_list_to_bridi(bridi, transforms);
    let linkargs = sub_state.arglist.args.clone();
    Ok(Box::new(move |extra_args: &crate::parse_m::Args| {
        let combined_args = crate::parse_m::join_args(extra_args, &linkargs);
        bridi(&combined_args)
    }))
}

// Ported from: JboParse.hs :: parseSelbri3 (broda/brode/brodi alias detection)
fn brodv_alias(sb3: &Selbri3) -> Option<TanruUnit> {
    if let Selbri3::TanruHead(frees, tu, terms) = sb3 {
        if frees.is_empty() && terms.is_empty() {
            if let TanruUnit::TUBrivla(bv) = tu.as_ref() {
                if matches!(bv.as_str(), "broda" | "brode" | "brodi" | "brodo" | "brodu") {
                    return Some(TanruUnit::TUBrivla(bv.clone()));
                }
            }
        }
    }
    None
}

// Ported from: JboParse.hs :: parseSelbri3 (bridi binding alias extraction)
fn bridi_binding_alias(sb3: &Selbri3) -> Option<TanruUnit> {
    match sb3 {
        Selbri3::BridiBinding(left, right) => brodv_alias(right).or_else(|| brodv_alias(left)),
        Selbri3::TanruHead(_, tu, _) => match tu.as_ref() {
            TanruUnit::TUSelbri3(inner) => bridi_binding_alias(inner),
            _ => None,
        },
        _ => None,
    }
}

// Ported from: JboParse.hs :: parseSelbri3 (bridi binding primary extraction)
fn bridi_binding_primary(sb3: &Selbri3) -> Option<&Selbri3> {
    match sb3 {
        Selbri3::BridiBinding(left, right) => {
            if brodv_alias(right).is_some() {
                Some(left)
            } else if brodv_alias(left).is_some() {
                Some(right)
            } else {
                None
            }
        }
        Selbri3::TanruHead(_, tu, _) => match tu.as_ref() {
            TanruUnit::TUSelbri3(inner) => bridi_binding_primary(inner),
            _ => None,
        },
        _ => None,
    }
}

// Ported from: JboParse.hs :: parseSelbri3
/// Parse a Selbri3 into a bridi
fn parse_selbri3(sb3: &Selbri3, bridi_state: &mut crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Bridi, String> {
    match sb3 {
        Selbri3::SBTanru(sb_left, sb_right) => {
            let alias = bridi_binding_alias(sb_right);
            let seltau_bridi = parse_seltau_selbri3(sb_left, bridi_state, state)?;
            let tertau_bridi = parse_selbri3(sb_right, bridi_state, state)?;
            let bridi = apply_seltau(seltau_bridi, tertau_bridi)?;
            if let Some(alias) = alias {
                let shared: crate::parse_m::StoredBridi = std::sync::Arc::new(move |args| bridi(args));
                state.bribasti_bindings.insert(alias, shared.clone());
                Ok(Box::new(move |args| shared(args)))
            } else {
                Ok(bridi)
            }
        }
        Selbri3::ConnectedSB(_fore, con, sb, sb3) => {
            let p = selbri_to_seltau_rel(sb, state)?;
            let p_prime = selbri_to_seltau_rel(&crate::jbo_syntax::Selbri::Selbri2(
                crate::jbo_syntax::Selbri2::Selbri3(*sb3.clone()),
            ), state)?;
            Ok(jbo_rel_to_bridi(JboRel::TanruConnective(
                con.clone(),
                Box::new(p),
                Box::new(p_prime),
            )))
        }
        Selbri3::BridiBinding(sb3_left, sb3_right) => {
            if let Some(alias) = brodv_alias(sb3_right) {
                let bridi = parse_selbri3(sb3_left, bridi_state, state)?;
                let shared: crate::parse_m::StoredBridi = std::sync::Arc::new(move |args| bridi(args));
                state.bribasti_bindings.insert(alias, shared.clone());
                Ok(Box::new(move |args| shared(args)))
            } else if let Some(alias) = brodv_alias(sb3_left) {
                let bridi = parse_selbri3(sb3_right, bridi_state, state)?;
                let shared: crate::parse_m::StoredBridi = std::sync::Arc::new(move |args| bridi(args));
                state.bribasti_bindings.insert(alias, shared.clone());
                Ok(Box::new(move |args| shared(args)))
            } else {
                let bridi = parse_selbri3(sb3_left, bridi_state, state)?;
                let _ = parse_selbri3(sb3_right, bridi_state, state)?;
                Ok(bridi)
            }
        }
        Selbri3::ScalarNegatedSB(nahe, sb3_inner) => {
            let closed_bridi = parsed_selbri_to_new_selbri(bridi_state, state, |sub_state, state| {
                parse_selbri3(sb3_inner, sub_state, state)
            })?;
            let nahe_owned = nahe.clone();
            Ok(map_rels_in_bridi(
                move |rel| JboRel::ScalarNegatedRel(nahe_owned.clone(), Box::new(rel)),
                closed_bridi,
            ))
        }
        Selbri3::TanruHead(frees, tu, terms) => {
            crate::parse_m::advance_arg_pos_to_selbri(bridi_state);
            let bridi = parse_tu(tu, bridi_state, state)?;
            parse_terms(terms, bridi_state, state)?;
            do_frees_in_parse_m(frees, state)?;
            Ok(bridi)
        }
    }
}

// Ported from: JboParse.hs :: parseTU
/// Parse a tanru unit into a bridi
fn parse_tu(tu: &TanruUnit, bridi_state: &mut crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Bridi, String> {
    match tu {
        TanruUnit::TUBrivla(_) => {
            Ok(crate::parse_m::get_bribasti_binding(&state.bribasti_bindings, tu))
        }
        TanruUnit::TUZei(vs) => {
            // Zei compound
            let compound = vs.join("-zei-");
            Ok(jbo_rel_to_bridi(JboRel::Brivla(compound)))
        }
        TanruUnit::TUGOhA(goha, _n) => {
            if goha == "du" {
                Ok(jbo_rel_to_bridi(JboRel::Equal))
            } else if tu_is_buha(tu) {
                let m = JboMex::MexNumeralString(vec![crate::jbo_syntax::Numeral::PA("su'o".to_string())]);
                let rel = do_buha(&m, tu, state)?;
                Ok(jbo_rel_to_bridi(rel))
            } else {
                let bridi = crate::parse_m::get_bribasti_binding(&state.bribasti_bindings, tu);
                Ok(bridi)
            }
        }
        TanruUnit::TUBridiQ(kau) => {
            // Bridi question (mo)
            // Add bridi question to state and get the question relation
            let kau_depth = *kau;
            let rel = state.add_bridi_question(kau_depth);
            Ok(jbo_rel_to_bridi(rel))
        }
        TanruUnit::TUMe(s) => {
            // me sumti
            let o = parse_sumti(s, state)?;
            Ok(jbo_rel_to_bridi(JboRel::Among(Box::new(o))))
        }
        TanruUnit::TUMoi(s, moi) => {
            // moi/mei/si'e etc
            let o = parse_sumti(s, state)?;
            Ok(jbo_rel_to_bridi(JboRel::Moi(Box::new(o), moi.clone())))
        }
        TanruUnit::TUAbstraction(crate::jbo_syntax::Abstractor::NegatedAbstractor(inner), ss) => {
            state.map_prop(crate::parse_m::PropTransform::Negate);
            parse_tu(&TanruUnit::TUAbstraction(*inner.clone(), ss.clone()), bridi_state, state)
        }
        TanruUnit::TUAbstraction(crate::jbo_syntax::Abstractor::LogConnectedAbstractor(lcon, abs1, abs2), ss) => {
            let left = parse_tu(&TanruUnit::TUAbstraction(*abs1.clone(), ss.clone()), bridi_state, state)?;
            let right = parse_tu(&TanruUnit::TUAbstraction(*abs2.clone(), ss.clone()), bridi_state, state)?;
            let lcon = lcon.clone();
            Ok(Box::new(move |args| {
                crate::jbo_prop::conn_to_fol(&lcon, left(args), right(args))
            }))
        }
        TanruUnit::TUAbstraction(crate::jbo_syntax::Abstractor::JoiConnectedAbstractor(joik, abs1, abs2), ss) => {
            let left = parse_tu(&TanruUnit::TUAbstraction(*abs1.clone(), ss.clone()), bridi_state, state)?;
            let right = parse_tu(&TanruUnit::TUAbstraction(*abs2.clone(), ss.clone()), bridi_state, state)?;
            let joik = joik.clone();
            Ok(Box::new(move |args| {
                crate::jbo_prop::joik_to_fol(joik.clone(), left(args), right(args))
            }))
        }
        TanruUnit::TUAbstraction(abs, ss) => {
            let abs_name = match abs {
                crate::jbo_syntax::Abstractor::NU(cmavo) => cmavo.clone(),
                _ => "nu".to_string(),
            };

            match abs_name.as_str() {
                "ka" | "ni" => {
                    let mut sub_bridi_state = crate::parse_m::BridiParseState::new();
                    sub_bridi_state.is_sub_bridi = true;
                    let outer_transforms = std::mem::take(&mut state.pending_prop_transforms);
                    let bridi_result = with_questions_scoped_bridi(false, state, |state| {
                        crate::parse_m::shunt_lambdas(state, 1);
                        parse_subsentence(ss, &mut sub_bridi_state, state)
                    });
                    state.pending_prop_transforms = outer_transforms;
                    let bridi = bridi_result?;
                    let saved_args = sub_bridi_state.arglist.args.clone();
                    let partial_bridi: Bridi = Box::new(move |extra_args| {
                        let joined = crate::parse_m::join_args(extra_args, &saved_args);
                        bridi(&joined)
                    });
                    let vpred = crate::parse_m::bridi_to_jbo_vpred(partial_bridi);
                    let npred = crate::parse_m::do_lambdas(vpred, state);
                    Ok(jbo_rel_to_bridi(JboRel::AbsPred(abs_name, npred)))
                }
                "poi'i" => {
                    let pred = with_questions_scoped(false, state, |state| subsent_to_pred(ss, state))?;
                    Ok(jbo_rel_to_bridi(JboRel::AbsPred(abs_name, crate::jbo_prop::pred_to_n_pred(pred))))
                }
                _ => {
                    let mut sub_bridi_state = crate::parse_m::BridiParseState::new();
                    sub_bridi_state.is_sub_bridi = true;
                    let outer_transforms = std::mem::take(&mut state.pending_prop_transforms);
                    let bridi_result = with_questions_scoped_bridi(false, state, |state| {
                        parse_subsentence(ss, &mut sub_bridi_state, state)
                    });
                    state.pending_prop_transforms.extend(outer_transforms);
                    let bridi = bridi_result?;
                    let saved_args = sub_bridi_state.arglist.args.clone();
                    let prop = bridi(&crate::parse_m::join_args(&crate::parse_m::Args::new(), &saved_args));
                    Ok(jbo_rel_to_bridi(JboRel::AbsProp(abs_name, Box::new(prop))))
                }
            }
        }
        TanruUnit::TUPermuted(n, inner_tu) => {
            let inner_bridi = parse_tu(inner_tu, bridi_state, state)?;
            let n_copy = *n;
            Ok(Box::new(move |args: &crate::parse_m::Args| {
                let swapped_args = crate::parse_m::swap_args(
                    &crate::parse_m::ArgPos::NPos(1),
                    &crate::parse_m::ArgPos::NPos(n_copy),
                    args,
                );
                inner_bridi(&swapped_args)
            }))
        }
        TanruUnit::TUJai(tag_opt, inner_tu) => {
            let inner_bridi = parse_tu(inner_tu, bridi_state, state)?;
            let jai_bridi = if let Some(tag) = tag_opt {
                let jtag = parse_tag_helper(tag, state)?;
                crate::parse_m::with_jai_as_tag(jtag, inner_bridi)
            } else {
                crate::parse_m::with_jai_as_raising(inner_bridi)
            };
            Ok(Box::new(move |args: &crate::parse_m::Args| {
                let swapped_args = crate::parse_m::swap_args(
                    &crate::parse_m::ArgPos::NPos(1),
                    &crate::parse_m::ArgPos::JaiPos,
                    args,
                );
                jai_bridi(&swapped_args)
            }))
        }
        TanruUnit::TUOperator(op) => {
            // Operator as selbri
            let jbo_op = parse_operator(op, state)?;
            Ok(jbo_rel_to_bridi(JboRel::OperatorRel(Box::new(jbo_op))))
        }
        TanruUnit::TUXOhI(tag) => {
            let jtag = parse_tag_helper(tag, state)?;
            Ok(jbo_rel_to_bridi(JboRel::TagRel(jtag)))
        }
        TanruUnit::TUSelbri3(sb3) => {
            parse_selbri3(sb3, bridi_state, state)
        }
    }
}


// Rust-only: Fill terms into a proposition (helper for term application)
fn fill_terms_in_prop(prop: JboProp, terms: &[JboTerm]) -> JboProp {
    match prop {
        Prop::Rel(rel, _) => Prop::Rel(rel, terms.to_vec()),
        Prop::Not(p) => Prop::Not(Box::new(fill_terms_in_prop(*p, terms))),
        Prop::Connected(c, p1, p2) => Prop::Connected(
            c,
            Box::new(fill_terms_in_prop(*p1, terms)),
            Box::new(fill_terms_in_prop(*p2, terms)),
        ),
        Prop::NonLogConnected(j, p1, p2) => Prop::NonLogConnected(
            j,
            Box::new(fill_terms_in_prop(*p1, terms)),
            Box::new(fill_terms_in_prop(*p2, terms)),
        ),
        Prop::Modal(m, p) => Prop::Modal(m, Box::new(fill_terms_in_prop(*p, terms))),
        Prop::Quantified(q, pred_opt, body_fn) => {
            // Logic.hs represents quantified bodies as functions; term filling only descends into realized propositions.
            Prop::Quantified(q, pred_opt, body_fn)
        }
        Prop::Eet => Prop::Eet,
    }
}

// Ported from: JboParse.hs :: parseSelbri (linkargs application helper)
fn apply_linkargs_to_bridi(bridi: Bridi, linkargs: crate::parse_m::Args) -> Bridi {
    Box::new(move |args: &crate::parse_m::Args| {
        let combined_args = crate::parse_m::join_args(args, &linkargs);
        bridi(&combined_args)
    })
}

// Ported from: JboParse.hs :: applySeltau
/// Apply a seltau (modifier) to a tertau (head)
fn apply_seltau(seltau: Bridi, tertau: Bridi) -> Result<Bridi, String> {
    let stpred = crate::parse_m::bridi_to_jbo_vpred(seltau);
    let seltau_rel = JboRel::VPredRel(stpred);

    Ok(Box::new(move |args: &crate::parse_m::Args| {
        let tertau_prop = tertau(args);
        apply_tanru_rel_to_prop(seltau_rel.clone(), tertau_prop)
    }))
}

// Ported from: JboParse.hs :: applySeltau (tanru relation application to proposition)
fn apply_tanru_rel_to_prop(seltau_rel: JboRel, prop: JboProp) -> JboProp {
    match prop {
        Prop::Rel(rel, terms) => {
            // Create tanru by combining seltau and tertau relations
            let tanru_rel = JboRel::Tanru(
                Box::new(seltau_rel),
                Box::new(rel)
            );
            Prop::Rel(tanru_rel, terms)
        }
        Prop::Not(p) => Prop::Not(Box::new(apply_tanru_rel_to_prop(seltau_rel, *p))),
        Prop::Connected(c, p1, p2) => {
            Prop::Connected(
                c,
                Box::new(apply_tanru_rel_to_prop(seltau_rel.clone(), *p1)),
                Box::new(apply_tanru_rel_to_prop(seltau_rel, *p2)),
            )
        }
        Prop::NonLogConnected(j, p1, p2) => {
            Prop::NonLogConnected(
                j,
                Box::new(apply_tanru_rel_to_prop(seltau_rel.clone(), *p1)),
                Box::new(apply_tanru_rel_to_prop(seltau_rel, *p2)),
            )
        }
        Prop::Modal(m, p) => Prop::Modal(m, Box::new(apply_tanru_rel_to_prop(seltau_rel, *p))),
        Prop::Quantified(q, pred_opt, body_fn) => {
            let seltau_for_restriction = seltau_rel.clone();
            let seltau_for_body = seltau_rel.clone();
            let new_pred = pred_opt.map(move |pred| {
                let seltau_rel = seltau_for_restriction.clone();
                std::rc::Rc::new(move |v| apply_tanru_rel_to_prop(seltau_rel.clone(), pred(v))) as std::rc::Rc<dyn Fn(i32) -> JboProp>
            });
            Prop::Quantified(q, new_pred, std::rc::Rc::new(move |v| apply_tanru_rel_to_prop(seltau_for_body.clone(), body_fn(v))))
        }
        Prop::Eet => Prop::Eet,
    }
}

// Ported from: JboParse.hs :: mapRelsInProp
fn map_rels_in_prop<F>(f: F, prop: JboProp) -> JboProp
where
    F: Fn(JboRel) -> JboRel + Clone + 'static,
{
    match prop {
        Prop::Rel(rel, terms) => Prop::Rel(f(rel), terms),
        Prop::Not(p) => Prop::Not(Box::new(map_rels_in_prop(f.clone(), *p))),
        Prop::Connected(c, p1, p2) => Prop::Connected(
            c,
            Box::new(map_rels_in_prop(f.clone(), *p1)),
            Box::new(map_rels_in_prop(f, *p2)),
        ),
        Prop::NonLogConnected(j, p1, p2) => Prop::NonLogConnected(
            j,
            Box::new(map_rels_in_prop(f.clone(), *p1)),
            Box::new(map_rels_in_prop(f, *p2)),
        ),
        Prop::Modal(m, p) => Prop::Modal(m, Box::new(map_rels_in_prop(f, *p))),
        Prop::Quantified(q, pred_opt, body_fn) => {
            let f_for_restriction = f.clone();
            let f_for_body = f.clone();
            let new_pred = pred_opt.map(move |pred| {
                let f = f_for_restriction.clone();
                std::rc::Rc::new(move |v| map_rels_in_prop(f.clone(), pred(v))) as std::rc::Rc<dyn Fn(i32) -> JboProp>
            });
            Prop::Quantified(q, new_pred, std::rc::Rc::new(move |v| map_rels_in_prop(f_for_body.clone(), body_fn(v))))
        }
        Prop::Eet => Prop::Eet,
    }
}

// Rust-only: Swap argument positions in permutation vector
fn swap_arg_positions(mut perm: Vec<usize>, i: usize, j: usize) -> Vec<usize> {
    if i < perm.len() && j < perm.len() {
        perm.swap(i, j);
    }
    perm
}

// Ported from: JboParse.hs :: parseSumti (partial implementation)
/// Parse a sumti expression
// Ported from: JboParse.hs :: parseSumti (lines 354-431)
/// Parse a sumti (argument)
/// Original Haskell handles:
/// - ConnectedSumti (logical/non-logical connectives)
/// - QAtom (quantified atom with frees and relative clauses)
/// - QSelbri (quantified selbri as sumti)
fn parse_sumti(s: &Sumti, state: &mut ParseState) -> Result<JboTerm, String> {
    let (mut o, mut jrels) = match s {
        Sumti::ConnectedSumti(forethought, conn, sumti1, sumti2, rels) => {
            // Connected sumti with logical or non-logical connective
            let o = match conn {
                Connective::JboConnLog(mtag, lcon) => {
                    let fresh = state.get_fresh_var(VariableDomain::UnrestrictedDomain);
                    let start = state.pending_prop_transforms.len();

                    let (o1, left_transforms, o2, right_transforms) = if let Some(tag) = mtag {
                        let v = quantify_helper(
                            &crate::jbo_syntax::Mex::MexNumeralString(vec![
                                crate::jbo_syntax::Numeral::PA("su'o".to_string())
                            ]),
                            None,
                            state,
                        )?;
                        let start = state.pending_prop_transforms.len();

                        if *forethought {
                            let jtag = parse_tag_helper(tag, state)?;
                            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), state);
                            let o1 = parse_sumti(sumti1, state)?;
                            let left_transforms = state.pending_prop_transforms.split_off(start);
                            do_tag_helper(&jtag, Some(v), state)?;
                            let o2 = parse_sumti(sumti2, state)?;
                            let right_transforms = state.pending_prop_transforms.split_off(start);
                            (o1, left_transforms, o2, right_transforms)
                        } else if is_tense_tag(tag) {
                            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), state);
                            let o1 = parse_sumti(sumti1, state)?;
                            let left_transforms = state.pending_prop_transforms.split_off(start);
                            let jtag = parse_tag_helper(tag, state)?;
                            do_tag_helper(&jtag, Some(v), state)?;
                            let o2 = parse_sumti(sumti2, state)?;
                            let right_transforms = state.pending_prop_transforms.split_off(start);
                            (o1, left_transforms, o2, right_transforms)
                        } else {
                            let jtag = parse_tag_helper(tag, state)?;
                            do_tag_helper(&jtag, Some(v.clone()), state)?;
                            let o1 = parse_sumti(sumti1, state)?;
                            let left_transforms = state.pending_prop_transforms.split_off(start);
                            do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v), state);
                            let o2 = parse_sumti(sumti2, state)?;
                            let right_transforms = state.pending_prop_transforms.split_off(start);
                            (o1, left_transforms, o2, right_transforms)
                        }
                    } else {
                        let o1 = parse_sumti(sumti1, state)?;
                        let left_transforms = state.pending_prop_transforms.split_off(start);
                        let o2 = parse_sumti(sumti2, state)?;
                        let right_transforms = state.pending_prop_transforms.split_off(start);
                        (o1, left_transforms, o2, right_transforms)
                    };

                    if let JboTerm::Var(n) = &fresh {
                        state.variable_domains.insert(*n, VariableDomain::FiniteDomain(vec![o1.clone(), o2.clone()]));
                    }
                    state.map_prop(crate::parse_m::PropTransform::LogicalConnectBranches {
                        conn: lcon.clone(),
                        fresh: fresh.clone(),
                        left_term: o1,
                        left_transforms,
                        right_term: o2,
                        right_transforms,
                    });

                    fresh
                }
                Connective::JboConnJoik(mtag, joik) => {
                    // Non-logical connective (JOI)
                    if mtag.is_some() {
                        // Warning: ignoring tag on non-logically connected sumti
                    }
                    let o1 = parse_sumti(sumti1, state)?;
                    let o2 = parse_sumti(sumti2, state)?;
                    JboTerm::JoikedTerms(joik.clone(), Box::new(o1), Box::new(o2))
                }
            };

            let jrels = parse_rels(rels, state)?;
            (o, jrels)
        }

        Sumti::QAtom { frees, quant, rels, atom } => {
            // Quantified atom with frees and relative clauses

            // Run non-SEI frees (indicators, TO, etc.) before parsing
            let non_discursive: Vec<Free> = frees
                .iter()
                .filter(|f| !is_discursive_free(f))
                .cloned()
                .collect();
            do_frees_in_parse_m(&non_discursive, state)?;

            let mm = if let Some(q) = quant {
                Some(parse_mex(q, state)?)
            } else {
                None
            };

            // Parse the atom and its relative clauses
            let (mut o, mut jrels_from_atom) = match atom.as_ref() {
                SumtiAtom::Variable(_) => {
                    let jrels = parse_rels(rels, state)?;
                    let (rps, jrels_rest) = strip_fore_restrictives(&jrels);
                    let rps: Vec<JboPred> = rps.into_iter().cloned().collect();
                    let o = parse_variable(atom, &rps, mm.as_ref(), state)?;
                    (o, jrels_rest)
                }

                SumtiAtom::SumtiQ(kau) => {
                    // Sumti question
                    let jrels = parse_rels(rels, state)?;
                    let (rps, jrels_rest) = strip_fore_restrictives(&jrels);
                    let dom = crate::jbo_prop::and_m_pred(rps.into_iter().cloned().collect());
                    let o = crate::parse_m::add_sumti_question(*kau, dom.as_ref(), state);
                    (o, jrels_rest)
                }

                _ => {
                    // Regular sumti atom
                    let (o, ijrels) = parse_sumti_atom(atom, state)?;
                    let mut all_jrels = ijrels;
                    all_jrels.extend(parse_rels(rels, state)?);
                    (o, all_jrels)
                }
            };

            // Apply quantifier if present; variables are quantified by parse_variable.
            if !matches!(atom.as_ref(), SumtiAtom::Variable(_)) {
                if let Some(m) = mm.as_ref() {
                    // Check for tu'o (null quantifier)
                    let is_null = matches!(m, JboMex::MexNumeralString(ref ns)
                        if ns.len() == 1 && matches!(&ns[0], crate::jbo_syntax::Numeral::PA(s) if s == "tu'o"));

                    if !is_null {
                        let (rps, jrels_rest) = strip_fore_restrictives(&jrels_from_atom);
                        let mut all_preds = vec![crate::jbo_prop::is_among(o.clone())];
                        all_preds.extend(rps.into_iter().cloned());

                        crate::parse_m::update_referenced(&o, state);
                        let and_pred = crate::jbo_prop::and_m_pred(all_preds);
                        o = quantify(m, and_pred, state)?;
                        jrels_from_atom = jrels_rest;
                    }
                }
            }

            // Attach SEI/ti'o sides to this term
            let sei_frees: Vec<Free> = frees
                .iter()
                .filter(|f| is_discursive_free(f))
                .cloned()
                .collect();

            if !sei_frees.is_empty() {
                let sei_sides = process_discursive_frees(&sei_frees, state)?;
                if !sei_sides.is_empty() {
                    o = JboTerm::TermWithSides(Box::new(o), sei_sides);
                }
            }

            (o, jrels_from_atom)
        }

        Sumti::QSelbri { quant, rels, selbri } => {
            // Quantified selbri as sumti (lo broda)
            let m = match quant {
                Mex::ConnectedMex(fore, con @ Connective::JboConnLog(Some(_), _), m1, m2) => {
                    let jm1 = parse_mex(m1, state)?;
                    let jm2 = parse_mex(m2, state)?;
                    JboMex::ConnectedMex(*fore, parse_connective(con), Box::new(jm1), Box::new(jm2))
                }
                _ => parse_mex(quant, state)?,
            };
            let sr = with_questions_scoped(true, state, |state| selbri_to_pred(selbri, state))?;
            let parsed_rels = parse_rels(rels, state)?;
            let (rps, jrels) = strip_fore_restrictives(&parsed_rels);

            let mut all_preds = vec![sr];
            all_preds.extend(rps.into_iter().cloned());
            let and_pred = crate::jbo_prop::and_m_pred(all_preds);

            let o = quantify(&m, and_pred, state)?;
            (o, jrels)
        }
    };

    // Apply remaining relative clauses
    for jrel in jrels {
        o = do_rel(o, jrel, state)?;
    }

    crate::parse_m::update_referenced(&o, state);

    Ok(o)
}

// Ported from: JboParse.hs :: parseSumti (selbri to predicate conversion)
fn selbri_to_pred(sb: &Selbri, state: &mut ParseState) -> Result<JboPred, String> {
    let vpred = selbri_to_vpred(sb, state)?;
    Ok(crate::jbo_prop::v_pred_to_pred(vpred))
}

// Ported from: JboParse.hs :: parseSumtiAtom (lines 504-573)
/// Parse a sumti atom - complete implementation
/// Original Haskell handles all sumti atom types including:
/// - Description (le/lo/la with gadri semantics)
/// - QualifiedSumti (LAhE, NAhE_BO)
/// - MexLi/MexMex (numbers and mex expressions)
/// - Variables, anaphora (ri, ra, etc.)
/// - Names, quotes, prosumti
fn parse_sumti_atom(sa: &SumtiAtom, state: &mut ParseState) -> Result<(JboTerm, Vec<JboRelClause>), String> {
    // Handle ri backcount
    if gets_ri(sa) {
        state.push_backcount();
    }

    // Parse inner relative clauses for certain types
    let jrels = match sa {
        SumtiAtom::Description(gadri, _mis, _miq, _ssb, rels, _inner_rels) => {
            if gadri.len() > 1 && gadri.chars().nth(1) == Some('a') {
                vec![]
            } else {
                parse_rels(rels, state)?
            }
        }
        SumtiAtom::QualifiedSumti(_, rels, _) => parse_rels(rels, state)?,
        SumtiAtom::Name(_, rels, _) => parse_rels(rels, state)?,
        _ => vec![],
    };

    // Parse the main sumti atom
    let mut o = match sa {
        SumtiAtom::Description(
            gadri,
            inner_sumti,
            inner_quant,
            selbri_or_sumti,
            rels,
            inner_rels,
        ) => {
            // Full gadri semantics: le/lo/la
            let extra_irels = if gadri.len() > 1 && gadri.chars().nth(1) == Some('a') {
                parse_rels(rels, state)?
            } else {
                vec![]
            };

            // Parse inner quantifier as mei predicate if present
            let mmr: Option<JboPred> = if let Some(iq) = inner_quant {
                let m = parse_mex(iq, state)?;
                Some(std::sync::Arc::new(move |o: &JboTerm| -> JboProp {
                    Prop::Rel(
                        JboRel::Moi(Box::new(JboTerm::Value(m.clone())), "mei".to_string()),
                        vec![o.clone()],
                    )
                }))
            } else {
                None
            };

            // Parse selbri or sumti
            let sr = with_questions_scoped(true, state, |state| match selbri_or_sumti {
                crate::jbo_syntax::EitherSelbriSumti::Selbri(sb) => selbri_to_pred(sb, state),
                crate::jbo_syntax::EitherSelbriSumti::Sumti(s) => {
                    let parsed_s = parse_sumti(s, state)?;
                    Ok(crate::jbo_prop::is_among(parsed_s))
                }
            })?;

            let mut all_inner = extra_irels;
            all_inner.extend(parse_rels(inner_rels, state)?);
            if let Some(is) = inner_sumti {
                let inner_term = Term::Sumti(Tagged::Untagged, *is.clone());
                if let Some(jrel) = parse_goi_rel("ne", &inner_term, state, |p| JboRelClause::JRIncidental(p))? {
                    all_inner.push(jrel);
                }
            }
            let mut restrictive_preds = Vec::new();
            let mut incidental_preds = Vec::new();
            let mut ias = Vec::new();
            for rel in all_inner {
                match rel {
                    JboRelClause::JRRestrictive(p) => restrictive_preds.push(p),
                    JboRelClause::JRIncidental(p) => incidental_preds.push(p),
                    JboRelClause::JRAssign(a) => ias.push(a),
                }
            }

            let mut xorlo_ps = vec![sr];
            if let Some(mmr) = mmr {
                xorlo_ps.push(mmr);
            }
            xorlo_ps.extend(restrictive_preds);
            xorlo_ps.extend(incidental_preds);

            // Apply gadri semantics
            let gadri_char = gadri.chars().nth(1).unwrap_or('o');
            let mut o = match gadri_char {
                'o' => {
                    // lo - veridical constant
                    let o = state.get_fresh_constant();
                    crate::parse_m::do_incidentals(o, &xorlo_ps, state)
                }
                'e' => {
                    let o = state.get_fresh_constant();
                    let pred = nonveridicial_pred(crate::jbo_prop::and_pred(xorlo_ps));
                    crate::parse_m::do_incidentals(o, &vec![pred], state)
                }
                'a' => {
                    // la - named entity
                    // Create PredNamed with the full predicate
                    JboTerm::PredNamed(crate::jbo_prop::and_pred(xorlo_ps))
                }
                _ => return Err(format!("Unknown gadri: {}", gadri)),
            };

            for ia in ias {
                let assign = match ia {
                    Ok(atom) => crate::parse_m::Either::Left(atom),
                    Err(term) => crate::parse_m::Either::Right(term),
                };
                crate::parse_m::do_assign(o.clone(), &assign, state);
            }

            o
        }

        SumtiAtom::QualifiedSumti(qualifier, _, sumti) => {
            let inner = parse_sumti(sumti, state)?;
            JboTerm::QualifiedTerm(qualifier.clone(), Box::new(inner))
        }

        SumtiAtom::MexLi(m) => match m {
            Mex::ConnectedMex(forethought, Connective::JboConnLog(mtag, lcon), m1, m2) => {
                let fresh = state.get_fresh_var(VariableDomain::UnrestrictedDomain);
                let start = state.pending_prop_transforms.len();

                let (jm1, left_transforms, jm2, right_transforms) = if let Some(tag) = mtag {
                    let v = quantify_helper(
                        &crate::jbo_syntax::Mex::MexNumeralString(vec![
                            crate::jbo_syntax::Numeral::PA("su'o".to_string())
                        ]),
                        None,
                        state,
                    )?;
                    let start = state.pending_prop_transforms.len();

                    if *forethought {
                        let jtag = parse_tag_helper(tag, state)?;
                        do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), state);
                        let jm1 = parse_mex(m1, state)?;
                        let left_transforms = state.pending_prop_transforms.split_off(start);
                        do_tag_helper(&jtag, Some(v), state)?;
                        let jm2 = parse_mex(m2, state)?;
                        let right_transforms = state.pending_prop_transforms.split_off(start);
                        (jm1, left_transforms, jm2, right_transforms)
                    } else if is_tense_tag(tag) {
                        do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), state);
                        let jm1 = parse_mex(m1, state)?;
                        let left_transforms = state.pending_prop_transforms.split_off(start);
                        let jtag = parse_tag_helper(tag, state)?;
                        do_tag_helper(&jtag, Some(v), state)?;
                        let jm2 = parse_mex(m2, state)?;
                        let right_transforms = state.pending_prop_transforms.split_off(start);
                        (jm1, left_transforms, jm2, right_transforms)
                    } else {
                        let jtag = parse_tag_helper(tag, state)?;
                        do_tag_helper(&jtag, Some(v.clone()), state)?;
                        let jm1 = parse_mex(m1, state)?;
                        let left_transforms = state.pending_prop_transforms.split_off(start);
                        do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v), state);
                        let jm2 = parse_mex(m2, state)?;
                        let right_transforms = state.pending_prop_transforms.split_off(start);
                        (jm1, left_transforms, jm2, right_transforms)
                    }
                } else {
                    let jm1 = parse_mex(m1, state)?;
                    let left_transforms = state.pending_prop_transforms.split_off(start);
                    let jm2 = parse_mex(m2, state)?;
                    let right_transforms = state.pending_prop_transforms.split_off(start);
                    (jm1, left_transforms, jm2, right_transforms)
                };

                if let JboTerm::Var(n) = &fresh {
                    state.variable_domains.insert(
                        *n,
                        VariableDomain::FiniteDomain(vec![
                            JboTerm::Value(jm1.clone()),
                            JboTerm::Value(jm2.clone()),
                        ]),
                    );
                }
                state.map_prop(crate::parse_m::PropTransform::LogicalConnectBranches {
                    conn: lcon.clone(),
                    fresh: fresh.clone(),
                    left_term: JboTerm::Value(jm1),
                    left_transforms,
                    right_term: JboTerm::Value(jm2),
                    right_transforms,
                });
                fresh
            }
            _ => {
                let parsed_m = parse_mex(m, state)?;
                if let Some((conn, left_m, right_m)) = logical_connected_mex_branches(&parsed_m) {
                    let fresh = state.get_fresh_var(VariableDomain::FiniteDomain(vec![
                        JboTerm::Value(left_m.clone()),
                        JboTerm::Value(right_m.clone()),
                    ]));
                    state.map_prop(crate::parse_m::PropTransform::LogicalConnectBranches {
                        conn,
                        fresh: fresh.clone(),
                        left_term: JboTerm::Value(left_m),
                        left_transforms: Vec::new(),
                        right_term: JboTerm::Value(right_m),
                        right_transforms: Vec::new(),
                    });
                    fresh
                } else {
                    JboTerm::Value(parsed_m)
                }
            }
        },
        SumtiAtom::MexMex(m) => JboTerm::TheMex(m.clone()),

        SumtiAtom::RelVar(n) => {
            state.var_bindings.get(sa).cloned().unwrap_or(JboTerm::Var(*n))
        }

        SumtiAtom::LambdaVar(lambda, number) => {
            // Lambda variable (ce'u)
            state.put_lambda(*lambda, *number)
        }

        SumtiAtom::Ri(n) => {
            // ri - anaphora to previous sumti
            state.get_sumbasti(sa)
        }

        SumtiAtom::Ra(cmavo) => {
            // ra/ru/ri - anaphora
            state.get_sumbasti(sa)
        }

        SumtiAtom::Assignable(n) => {
            // ko'a, fo'a, etc. - assignable pronouns
            state.get_sumbasti(sa)
        }

        SumtiAtom::LerfuString(ls) => {
            // Lerfu string - assignable
            state.get_sumbasti(sa)
        }

        SumtiAtom::MainBridiSumbasti(n) => {
            // ce'u, etc. - main bridi sumbasti
            state.get_sumbasti(sa)
        }

        SumtiAtom::Zohe => {
            // zo'e - fresh constant
            state.get_fresh_constant()
        }

        SumtiAtom::NonAnaphoricProsumti(ps) => {
            // ti, ta, tu, etc. - non-anaphoric prosumti
            JboTerm::NonAnaph(ps.clone())
        }

        SumtiAtom::Name(gadri, _, name) => {
            // la <name> - named constant
            // JboParse.hs :: parseSumtiAtom also returns `Named s` here and carries the gadri into the later lai/lei/loi/la'i/le'i/lo'i branch.
            JboTerm::Named(name.clone())
        }

        SumtiAtom::Quote(text) => {
            JboTerm::JboQuote(eval_texticules(text))
        }

        SumtiAtom::ErrorQuote(vs) => {
            JboTerm::JboErrorQuote(vs.clone())
        }

        SumtiAtom::NonJboQuote(s) => {
            JboTerm::JboNonJboQuote(s.clone())
        }

        SumtiAtom::Word(s) => {
            JboTerm::Valsi(s.clone())
        }

        SumtiAtom::Variable(n) => {
            // Should not reach here - handled in parse_sumti
            return Err("Variable should be handled in parse_sumti".to_string());
        }

        SumtiAtom::SumtiQ(kau) => {
            // Should not reach here - handled in parse_sumti
            return Err("SumtiQ should be handled in parse_sumti".to_string());
        }

        SumtiAtom::SelbriVar => {
            // Selbri variables are relation variables in JboParse.hs; the sumti-term fallback leaves no term value.
            JboTerm::Unfilled
        }
    };

    // Handle lai/lei/loi/la'i/le'i/lo'i - collective/mass gadri
    if let SumtiAtom::Description(gadri, ..) | SumtiAtom::Name(gadri, ..) = sa {
        if gadri.len() > 2 {
            let suffix = &gadri[2..];
            if suffix == "i" || suffix == "'i" {
                // Collective (gunma) or mass (selcmi)
                let o_prime = state.get_fresh_constant();
                let collector = if suffix == "i" {
                    JboRel::Brivla("gunma".to_string())
                } else {
                    JboRel::Brivla("selcmi".to_string())
                };
                let o_clone = o.clone();
                let pred: JboPred = std::sync::Arc::new(move |x| {
                    Prop::Rel(collector.clone(), vec![x.clone(), o_clone.clone()])
                });
                o = crate::parse_m::do_incidentals(o_prime, &vec![pred], state);
            }
        }
    }

    // Update sumbasti with this sumti atom
    state.update_sumbasti_with_sumti_atom(sa, &o);

    Ok((o, jrels))
}

// Ported from: JboParse.hs :: parseSumtiAtom (ri backcount detection)
pub fn gets_ri(sa: &SumtiAtom) -> bool {
    crate::jbo_syntax::gets_ri(sa)
}

// Ported from: JboParse.hs :: quantify
/// Quantify a term with a mex quantifier
/// Original Haskell:
///   quantify :: PreProp r => JboMex -> Maybe JboPred -> ParseM r JboTerm
///   quantify m r = do
///       fresh <- getFreshVar $ maybe UnrestrictedDomain PredDomain r
///       mapProp $ \p ->
///           Quantified (terpJboMexAsQuantifier m) (jboPredToLojPred <$> r) $
///               \v -> subTerm fresh (BoundVar v) p
///       return fresh
fn quantify(
    m: &JboMex,
    r: Option<JboPred>,
    state: &mut ParseState,
) -> Result<JboTerm, String> {
    // Get fresh variable
    // Ported from: JboParse.hs :: quantify
    //   fresh <- getFreshVar $ maybe UnrestrictedDomain PredDomain r
    let domain = match &r {
        Some(pred) => VariableDomain::PredDomain(pred.clone()),
        None => VariableDomain::UnrestrictedDomain,
    };
    let fresh = state.get_fresh_var(domain);

    let restriction = r.map(|pred| {
        std::rc::Rc::new(move |v: i32| pred(&JboTerm::BoundVar(v))) as std::rc::Rc<dyn Fn(i32) -> crate::parse_m::JboProp>
    });

    match m {
        JboMex::ConnectedMex(forethought, Connective::JboConnLog(mtag, lcon), m1, m2) => {
            if let Some(tag) = mtag {
                let jtag = parse_tag_helper(tag, state)?;
                let is_tense = is_tense_tag(tag);
                let event = if *forethought || is_tense {
                    state.get_fresh_var(VariableDomain::UnrestrictedDomain)
                } else {
                    fresh.clone()
                };
                let (event_fresh, left_modal, right_modal) = if *forethought || is_tense {
                    (
                        Some(event.clone()),
                        Some(crate::jbo_prop::JboModalOp::WithEventAs(event.clone())),
                        Some(crate::jbo_prop::JboModalOp::Tagged(jtag, Some(event.clone()))),
                    )
                } else {
                    (
                        None,
                        Some(crate::jbo_prop::JboModalOp::Tagged(jtag, Some(event.clone()))),
                        Some(crate::jbo_prop::JboModalOp::WithEventAs(event.clone())),
                    )
                };
                state.map_prop(crate::parse_m::PropTransform::ConnectQuantifyTagged {
                    conn: lcon.clone(),
                    fresh: fresh.clone(),
                    left_quant: terp_jbo_mex_as_quantifier_from_jbo(m1),
                    right_quant: terp_jbo_mex_as_quantifier_from_jbo(m2),
                    restriction,
                    event_fresh,
                    left_modal,
                    right_modal,
                });
            } else {
                state.map_prop(crate::parse_m::PropTransform::ConnectQuantify {
                    conn: lcon.clone(),
                    fresh: fresh.clone(),
                    left_quant: terp_jbo_mex_as_quantifier_from_jbo(m1),
                    right_quant: terp_jbo_mex_as_quantifier_from_jbo(m2),
                    restriction,
                });
            }
        }
        _ => {
            state.map_prop(crate::parse_m::PropTransform::Quantify(
                fresh.clone(),
                terp_jbo_mex_as_quantifier_from_jbo(m),
                restriction,
            ));
        }
    }


    Ok(fresh)
}

// Ported from: JboParse.hs :: rquantify
/// Quantify a relation variable with a mex quantifier
/// Original Haskell:
///   rquantify :: PreProp r => JboMex -> ParseM r JboRel
///   rquantify m = do
///       fresh <- getFreshRVar
///       mapProp $ \p ->
///           Quantified (RelQuantifier $ terpJboMexAsQuantifier m) Nothing $
///               \v -> subRel fresh (BoundRVar v) p
///       return fresh
fn rquantify(
    m: &JboMex,
    state: &mut ParseState,
) -> Result<JboRel, String> {
    // Get fresh relation variable
    let fresh = state.get_fresh_rvar();

    // Get quantifier and wrap as RelQuantifier
    let quantifier = terp_jbo_mex_as_quantifier_from_jbo(m);
    let rel_quantifier = JboQuantifier::RelQuantifier(Box::new(quantifier));

    state.map_prop(crate::parse_m::PropTransform::RQuantify(
        fresh.clone(),
        rel_quantifier,
    ));

    Ok(fresh)
}

// Ported from: JboParse.hs :: parseRels
/// Parse relative clauses
/// Original Haskell:
///   parseRels :: PreProp r => [RelClause] -> ParseM r [JboRelClause]
///   parseRels rels = concat . map maybeToList <$> mapM parseRel rels
fn parse_rels(rels: &[RelClause], state: &mut ParseState) -> Result<Vec<JboRelClause>, String> {
    let mut result = Vec::new();

    for rel in rels {
        if let Some(jrel) = parse_rel(rel, state)? {
            result.push(jrel);
        }
    }

    Ok(result)
}

// Ported from: JboParse.hs :: parseRel (single relative clause parsing)
fn parse_rel(rel: &RelClause, state: &mut ParseState) -> Result<Option<JboRelClause>, String> {
    match rel {
        RelClause::Restrictive(ss) => {
            // Restrictive relative clause: poi
            let pred = subsent_to_pred(ss, state)?;
            Ok(Some(JboRelClause::JRRestrictive(pred)))
        }
        RelClause::Descriptive(ss) => {
            let pred = subsent_to_pred(ss, state)?;
            Ok(Some(JboRelClause::JRRestrictive(nonveridicial_pred(pred))))
        }
        RelClause::Incidental(ss) => {
            // Incidental relative clause: voi
            let pred = subsent_to_pred(ss, state)?;
            Ok(Some(JboRelClause::JRIncidental(pred)))
        }
        RelClause::RestrictiveGOI(goi, term) => {
            // GOI relative clause: pe, po, po'e, po'u
            parse_goi_rel(goi, term, state, |p| JboRelClause::JRRestrictive(p))
        }
        RelClause::IncidentalGOI(goi, term) => {
            // GOI relative clause: ne, no'u
            parse_goi_rel(goi, term, state, |p| JboRelClause::JRIncidental(p))
        }
        RelClause::Assignment(term) => {
            // Assignment relative clause: goi
            parse_assignment_rel(term, state)
        }
    }
}

// Ported from: JboParse.hs :: subsentToPred
/// Convert subsentence to predicate
///
/// Original Haskell:
///   subsentToPred :: Subsentence -> ParseM r JboPred
///   subsentToPred ss = do
///       fresh <- getFreshVar UnrestrictedDomain
///       b <- partiallyRunSubBridiM $ do
///           modifyVarBindings $ setShunting RelVar fresh
///           parseSubsentence ss
///       reffed <- case fresh of
///           Var n -> referenced n
///           _ -> error "bad fresh var"
///       let p = bridiToJboVPred b $ if reffed then [] else [fresh]
///       return $ \o -> subTerm fresh o p
fn subsent_to_pred(ss: &Subsentence, state: &mut ParseState) -> Result<JboPred, String> {
    // Get fresh variable for ke'a (the implicit argument)
    let fresh = state.get_fresh_var(VariableDomain::UnrestrictedDomain);

    // Create a new BridiParseState for this subsentence
    let mut bridi_state = crate::parse_m::BridiParseState::new();

    let outer_transforms = std::mem::take(&mut state.pending_prop_transforms);
    let old_var_bindings = state.var_bindings.clone();
    state.var_bindings = crate::parse_m::set_shunting(
        &|n| SumtiAtom::RelVar(n as i32),
        fresh.clone(),
        &state.var_bindings,
    );

    // Parse the subsentence to get a bridi
    let bridi_result = parse_subsentence(ss, &mut bridi_state, state);
    state.var_bindings = old_var_bindings;
    state.pending_prop_transforms.extend(outer_transforms);
    let bridi = bridi_result?;

    let reffed = match &fresh {
        JboTerm::Var(n) => state.referenced(*n),
        _ => return Err("bad fresh var".to_string()),
    };

    let bridi = crate::parse_m::partially_resolve_bridi(bridi, &bridi_state.arglist.args);
    let fresh_clone = fresh.clone();
    Ok(std::sync::Arc::new(move |o: &JboTerm| {
        let args = if reffed {
            crate::parse_m::Args::new()
        } else {
            [(crate::parse_m::ArgPos::UnfilledPos(0), fresh_clone.clone())].into_iter().collect()
        };
        let prop = bridi(&args);
        crate::jbo_prop::sub_term(&fresh_clone, o, prop)
    }))
}

// Ported from: JboParse.hs :: parseSubsentence
/// Parse a subsentence (sentence with frees and prenex)
fn parse_subsentence(ss: &Subsentence, bridi_state: &mut crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Bridi, String> {
    do_frees_in_parse_m(&ss.frees, state)?;
    parse_prenex_terms(&ss.prenex, bridi_state, state)?;
    parse_sentence(&ss.sentence, bridi_state, state)
}

// Ported from: JboParse.hs :: parseSentence
/// Parse a sentence (terms + bridi tail)
fn parse_sentence(s: &Sentence, bridi_state: &mut crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Bridi, String> {
    // Parse terms first - this is where quantification happens
    parse_terms(&s.terms, bridi_state, state)?;

    // Parse bridi tail
    let bridi = parse_bridi_tail(&s.tail, bridi_state, state)?;

    // Apply any pending transforms (quantifiers, negations, modals)
    Ok(crate::parse_m::apply_transforms_to_bridi(bridi, state))
}

// Ported from: JboParse.hs :: parseBTail
/// Parse a bridi tail
// Ported from: JboParse.hs :: parseBTail (lines 154-186)
/// Parse a bridi tail - complete implementation
/// Original Haskell handles:
/// - GekSentence (forethought connectives, negation, tags)
/// - ConnectedBT (afterthought connectives)
/// - BridiTail3 (selbri with tail terms, including negation and tags)
/// - SBInverted (inverted tanru)
fn parse_bridi_tail(bt: &BridiTail, bridi_state: &mut crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Bridi, String> {
    match bt {
        BridiTail::GekSentence(gs) => {
            // Forethought connective sentence
            match gs.as_ref() {
                GekSentence::ConnectedGS(conn, subsentence1, subsentence2, tail_terms) => {
                    // Forethought connective: gi <ss1> gi <ss2>
                    let result = do_connective_with_bridi(
                        true, // forethought
                        conn,
                        |bridi_state, state| parse_subsentence(subsentence1, bridi_state, state),
                        |bridi_state, state| parse_subsentence(subsentence2, bridi_state, state),
                        bridi_state,
                        state,
                    )?;

                    // Parse tail terms
                    parse_terms(tail_terms, bridi_state, state)?;

                    Ok(result)
                }
                GekSentence::NegatedGS(inner_gs) => {
                    state.map_prop(crate::parse_m::PropTransform::Negate);
                    parse_bridi_tail(&BridiTail::GekSentence(inner_gs.clone()), bridi_state, state)
                }
                GekSentence::TaggedGS(tag, gek_sentence) => {
                    // Tagged gek sentence: <tag> gi ...
                    let tag_parsed = parse_tag_helper(tag, state)?;
                    do_bare_tag(&tag_parsed, state)?;
                    parse_bridi_tail(&BridiTail::GekSentence(gek_sentence.clone()), bridi_state, state)
                }
            }
        }

        BridiTail::ConnectedBT(conn, bridi_tail1, bridi_tail2, tail_terms) => {
            let result = do_connective_with_bridi(
                false,
                conn,
                |bridi_state, state| parse_bridi_tail(bridi_tail1, bridi_state, state),
                |bridi_state, state| parse_bridi_tail(bridi_tail2, bridi_state, state),
                bridi_state,
                state,
            )?;
            parse_terms(tail_terms, bridi_state, state)?;
            Ok(result)
        }

        BridiTail::BridiTail3(selbri, tail_terms) => {
            // Handle negation and tags at bridi tail level
            match selbri {
                Selbri::Negated(sb) => {
                    state.map_prop(crate::parse_m::PropTransform::Negate);
                    parse_bridi_tail(
                        &BridiTail::BridiTail3(*sb.clone(), tail_terms.clone()),
                        bridi_state,
                        state,
                    )
                }
                Selbri::TaggedSelbri(tag, inner_sb) => {
                    // Tagged selbri at bridi tail level
                    let tag_parsed = parse_tag_helper(tag, state)?;
                    do_bare_tag(&tag_parsed, state)?;
                    parse_bridi_tail(
                        &BridiTail::BridiTail3(*inner_sb.clone(), tail_terms.clone()),
                        bridi_state,
                        state,
                    )
                }
                Selbri::Selbri2(sb2) => {
                    // Check for inverted tanru (SBInverted)
                    match sb2 {
                        Selbri2::SBInverted(selbri3, selbri2) => {
                            // Inverted tanru: handle specially
                            parse_inverted_bridi_tail(selbri3, selbri2, tail_terms, bridi_state, state)
                        }
                        Selbri2::Selbri3(sb3) => {
                            if let (Some(alias), Some(primary)) = (bridi_binding_alias(sb3), bridi_binding_primary(sb3)) {
                                let bridi = parse_selbri3(primary, bridi_state, state)?;
                                parse_terms(tail_terms, bridi_state, state)?;
                                let captured_args = bridi_state.arglist.args.clone();
                                let shared: crate::parse_m::StoredBridi = std::sync::Arc::new(move |args| {
                                    let combined_args = crate::parse_m::join_args(args, &captured_args);
                                    bridi(&combined_args)
                                });
                                state.bribasti_bindings.insert(alias, shared.clone());
                                Ok(Box::new(move |args| shared(args)))
                            } else {
                                let bridi = parse_selbri(selbri, bridi_state, state)?;
                                parse_terms(tail_terms, bridi_state, state)?;
                                Ok(bridi)
                            }
                        }
                    }
                }
            }
        }
    }
}

// Ported from: JboParse.hs :: parseBTail (lines 178-183)
// Handles: BridiTail3 (Selbri2 (SBInverted sb sb')) tts
fn parse_inverted_bridi_tail(
    sb3: &Selbri3,
    sb2: &Selbri2,
    tail_terms: &[Term],
    bridi_state: &mut crate::parse_m::BridiParseState,
    state: &mut ParseState,
) -> Result<Bridi, String> {
    let tertau = parse_selbri3(sb3, bridi_state, state)?;

    let seltau = parse_seltau_bridi(bridi_state, state, |sub_state, state| {
        parse_inverted_seltau_bridi(sb2, tail_terms, sub_state, state)
    })?;

    apply_seltau(seltau, tertau)
}

// Ported from: JboParse.hs :: parseBTail (inverted tanru seltau parsing)
fn parse_inverted_seltau_bridi(
    sb2: &Selbri2,
    tail_terms: &[Term],
    bridi_state: &mut crate::parse_m::BridiParseState,
    state: &mut ParseState,
) -> Result<Bridi, String> {
    match sb2 {
        Selbri2::SBInverted(sb3_inner, sb2_inner) => {
            let tertau = parse_selbri3(sb3_inner, bridi_state, state)?;
            let seltau = parse_inverted_seltau_bridi(sb2_inner, tail_terms, bridi_state, state)?;
            apply_seltau(seltau, tertau)
        }
        Selbri2::Selbri3(sb3_inner) => {
            let wrapped = Selbri::Selbri2(Selbri2::Selbri3(Selbri3::TanruHead(
                Vec::new(),
                Box::new(TanruUnit::TUSelbri3(sb3_inner.clone())),
                tail_terms.to_vec(),
            )));
            parse_selbri(&wrapped, bridi_state, state)
        }
    }
}

// Ported from: JboParse.hs :: doTag (bare tag application without sumti)
fn do_bare_tag(tag: &crate::jbo_prop::JboTag, state: &mut ParseState) -> Result<(), String> {
    do_tag_helper(tag, None, state)
}


// Ported from: JboParse.hs :: parseRel (GOI relative clause parsing)
fn parse_goi_rel<F>(
    goi: &str,
    term: &Term,
    state: &mut ParseState,
    wrapper: F,
) -> Result<Option<JboRelClause>, String>
where
    F: Fn(JboPred) -> JboRelClause,
{
    let mut rel_bridi_state = crate::parse_m::BridiParseState::new();
    let rets = crate::parse_m::ignoring_args(&mut rel_bridi_state, |bridi_state| {
        parse_term(term, bridi_state, state)
    })?;

    match rets.into_iter().next() {
        Some(Either::Right(o)) => {
            let rel = match goi {
                "pe" | "ne" => JboRel::Brivla("srana".to_string()),
                "po'u" | "no'u" => JboRel::Equal,
                "po" => JboRel::Tanru(
                    Box::new(JboRel::VPredRel(std::sync::Arc::new(|os: &[JboTerm]| {
                        Prop::Rel(JboRel::Brivla("steci".to_string()), os.to_vec())
                    }))),
                    Box::new(JboRel::Brivla("srana".to_string())),
                ),
                "po'e" => JboRel::Tanru(
                    Box::new(JboRel::VPredRel(std::sync::Arc::new(|os: &[JboTerm]| {
                        Prop::Rel(JboRel::Brivla("jinzi".to_string()), os.to_vec())
                    }))),
                    Box::new(JboRel::Brivla("srana".to_string())),
                ),
                _ => return Ok(None),
            };
            let pred = std::sync::Arc::new(move |x: &JboTerm| {
                Prop::Rel(rel.clone(), vec![x.clone(), o.clone()])
            });
            Ok(Some(wrapper(pred)))
        }
        Some(Either::Left((jbotag, mo))) => {
            let pred = std::sync::Arc::new(move |x: &JboTerm| {
                Prop::Rel(
                    JboRel::TagRel(jbotag.clone()),
                    vec![mo.clone().unwrap_or(JboTerm::Unfilled), x.clone()],
                )
            });
            Ok(Some(wrapper(pred)))
        }
        _ => Ok(None),
    }
}

// Ported from: JboParse.hs :: parseRel (assignment relative clause parsing)
fn parse_assignment_rel(term: &Term, state: &mut ParseState) -> Result<Option<JboRelClause>, String> {
    // Check if it's a simple assignable sumti atom
    if let Term::Sumti(Tagged::Untagged, sumti) = term {
        if let Sumti::QAtom { atom, .. } = sumti {
            if is_assignable_atom(&**atom) {
                // Result<SumtiAtom, JboTerm> where Ok = SumtiAtom (Left in Haskell Either)
                return Ok(Some(JboRelClause::JRAssign(Ok((**atom).clone()))));
            }
        }
    }

    let mut rel_bridi_state = crate::parse_m::BridiParseState::new();
    let rets = crate::parse_m::ignoring_args(&mut rel_bridi_state, |bridi_state| {
        parse_term(term, bridi_state, state)
    })?;

    for ret in rets {
        if let Either::Right(o) = ret {
            return Ok(Some(JboRelClause::JRAssign(Err(o))));
        }
    }

    Ok(None)
}

// Ported from: JboParse.hs :: parseRel (assignable atom detection)
fn is_assignable_atom(atom: &SumtiAtom) -> bool {
    matches!(
        atom,
        SumtiAtom::Assignable(_) | SumtiAtom::LerfuString(_) | SumtiAtom::Name(_, _, _)
    )
}

// Ported from: JboParse.hs :: doRel
/// Apply a relative clause to a term
/// Original Haskell:
///   doRel :: PreProp r => JboTerm -> JboRelClause -> ParseM r JboTerm
///   doRel o (JRIncidental p) = reWrapTermSides o <$> doIncidental (stripTermSides o) p
///   doRel o (JRRestrictive p) = do
///       o' <- getFreshConstant
///       o' <- doIncidentals o' $ [p, isAmong (stripTermSides o)]
///       return o'
///   doRel o (JRAssign a) = reWrapTermSides o <$> doAssign (stripTermSides o) a
fn do_rel(o: JboTerm, jrel: JboRelClause, state: &mut ParseState) -> Result<JboTerm, String> {
    match jrel {
        JboRelClause::JRIncidental(p) => {
            // Incidental relative clause - add as side texticule
            let stripped = crate::jbo_prop::strip_term_sides(&o);
            let preds: Vec<JboPred> = vec![p];
            let result = crate::parse_m::do_incidentals(stripped, &preds, state);
            Ok(crate::jbo_prop::re_wrap_term_sides(&o, result))
        }
        JboRelClause::JRRestrictive(p) => {
            // Restrictive relative clause - create fresh constant with restrictions
            let o_prime = state.get_fresh_constant();
            let stripped = crate::jbo_prop::strip_term_sides(&o);
            let is_among_pred = crate::jbo_prop::is_among(stripped);
            let preds: Vec<JboPred> = vec![p, is_among_pred];
            let result = crate::parse_m::do_incidentals(o_prime, &preds, state);
            Ok(result)
        }
        JboRelClause::JRAssign(assign_result) => {
            // Assignment relative clause
            // Convert Result<SumtiAtom, JboTerm> to Either<SumtiAtom, JboTerm>
            let either_assign = match assign_result {
                Ok(atom) => crate::parse_m::Either::Left(atom),
                Err(term) => crate::parse_m::Either::Right(term),
            };
            let stripped = crate::jbo_prop::strip_term_sides(&o);
            let result = crate::parse_m::do_assign(stripped, &either_assign, state);
            Ok(crate::jbo_prop::re_wrap_term_sides(&o, result))
        }
    }
}

// ============================================================================
// MISSING FUNCTIONS - COMPREHENSIVE PORT FROM HASKELL
// ============================================================================

// Ported from: JboParse.hs :: FreeReturn
/// Return type for evalFree
enum FreeReturn {
    FRSides(crate::jbo_prop::SideType, Vec<crate::jbo_prop::Texticule>),
    FRTruthQ(Option<i32>),
    FRIgnored,
}

// Ported from: JboParse.hs :: evalFree (lines 81-87)
/// Evaluate a free modifier to determine its semantic effect
/// Original Haskell:
///   evalFree :: Free -> ParseStateM FreeReturn
///   evalFree (Discursive bt) = (FRSides SideDiscursive . (\p -> [TexticuleProp p]) <$>) $ evalParseM $
///       ($nullArgs) <$> partiallyRunBridiM (parseBTail bt)
///   evalFree (Bracketed text) = FRSides SideBracketed <$> evalText text
///   evalFree (TruthQ kau) = return $ FRTruthQ kau
///   evalFree _ = return FRIgnored
fn eval_free(free: &Free, state: &mut ParseState) -> Result<FreeReturn, String> {
    match free {
        Free::Discursive(bt) => {
            let outer_transforms = std::mem::take(&mut state.pending_prop_transforms);
            let mut bridi_state = crate::parse_m::BridiParseState::new();
            let bridi = parse_bridi_tail(bt, &mut bridi_state, state)?;
            let transforms = std::mem::take(&mut state.pending_prop_transforms);
            state.pending_prop_transforms = outer_transforms;
            let bridi = crate::parse_m::apply_transform_list_to_bridi(bridi, transforms);
            let bridi = crate::parse_m::partially_resolve_bridi(bridi, &bridi_state.arglist.args);
            let prop = bridi(&crate::parse_m::Args::new());
            let texticule = crate::jbo_prop::Texticule::TexticuleProp(prop);
            Ok(FreeReturn::FRSides(
                crate::jbo_prop::SideType::SideDiscursive,
                vec![texticule],
            ))
        }
        Free::Bracketed(text) => {
            Ok(FreeReturn::FRSides(
                crate::jbo_prop::SideType::SideBracketed,
                eval_texticules(text),
            ))
        }
        Free::TruthQ(kau) => {
            // Truth question
            Ok(FreeReturn::FRTruthQ(*kau))
        }
        _ => {
            // Ignore other frees
            Ok(FreeReturn::FRIgnored)
        }
    }
}

// Ported from: JboParse.hs :: doFree (lines 108-114)
/// Process a single free modifier
/// Original Haskell:
///   doFree :: Free -> ParseStateM ()
///   doFree f = do
///       fr <- evalFree f
///       case fr of
///           FRSides sideType jt -> mapM_ (addSideTexticule . TexticuleSide sideType) jt
///           FRTruthQ kau -> addQuestion $ Question kau QTruth
///           FRIgnored -> return ()
fn do_free(free: &Free, state: &mut ParseState) -> Result<(), String> {
    let fr = eval_free(free, state)?;
    match fr {
        FreeReturn::FRSides(side_type, jt) => {
            for texticule in jt {
                let texticule = match texticule {
                    crate::jbo_prop::Texticule::TexticuleProp(prop) => {
                        crate::parse_m::Texticule::TexticuleProp(prop)
                    }
                    crate::jbo_prop::Texticule::TexticuleSide(inner_side_type, inner) => {
                        crate::parse_m::Texticule::TexticuleSide(
                            inner_side_type,
                            Box::new(prop_texticule_to_parse(*inner)?),
                        )
                    }
                    crate::jbo_prop::Texticule::TexticuleFrag(_) => return Err("Unexpected fragment side texticule".to_string()),
                };
                state.add_side_texticule(crate::parse_m::Texticule::TexticuleSide(
                    side_type,
                    Box::new(texticule),
                ));
            }
            Ok(())
        }
        FreeReturn::FRTruthQ(kau) => {
            state.add_question(crate::parse_m::Question {
                kau_depth: kau,
                info: crate::parse_m::QInfo::QTruth,
            });
            Ok(())
        }
        FreeReturn::FRIgnored => Ok(()),
    }
}

// Ported from: JboParse.hs :: doFrees (lines 89-90)
/// Process a list of free modifiers
/// Original Haskell:
///   doFrees :: [Free] -> ParseStateM ()
///   doFrees = mapM_ doFree
fn do_frees(frees: &[Free], state: &mut ParseState) -> Result<(), String> {
    for free in frees {
        do_free(free, state)?;
    }
    Ok(())
}

// Ported from: JboParse.hs :: doFreesInParseM (lines 91-92)
/// Process frees in ParseM context (lifted from ParseStateM)
/// Original Haskell:
///   doFreesInParseM :: [Free] -> ParseM r ()
///   doFreesInParseM = liftParseStateMToParseM . doFrees
fn do_frees_in_parse_m(frees: &[Free], state: &mut ParseState) -> Result<(), String> {
    do_frees(frees, state)
}

// Ported from: JboParse.hs :: processDiscursiveFrees (lines 99-106)
/// Evaluate Discursive (SEI) frees to side texticules for attaching to terms
/// Original Haskell:
///   processDiscursiveFrees :: [Free] -> ParseM r [Texticule]
///   processDiscursiveFrees discursives = liftParseStateMToParseM $
///       fmap concat $ traverse (\f -> do
///           fr <- evalFree f
///           case fr of
///               FRSides SideDiscursive jt -> return [TexticuleSide SideDiscursive t | t <- jt]
///               _ -> return []) discursives
fn process_discursive_frees(
    discursives: &[Free],
    state: &mut ParseState,
) -> Result<Vec<crate::jbo_prop::Texticule>, String> {
    let mut result = Vec::new();
    for free in discursives {
        let fr = eval_free(free, state)?;
        if let FreeReturn::FRSides(crate::jbo_prop::SideType::SideDiscursive, jt) = fr {
            for t in jt {
                result.push(crate::jbo_prop::Texticule::TexticuleSide(
                    crate::jbo_prop::SideType::SideDiscursive,
                    Box::new(t),
                ));
            }
        }
    }
    Ok(result)
}

// Ported from: JboParse.hs :: parseStatements (lines 116-117)
/// Parse multiple statements and combine with logical AND
/// Original Haskell:
///   parseStatements :: [Statement] -> JboPropM JboProp
///   parseStatements ss = bigAnd <$> mapM parseStatement ss
fn parse_statements(stmts: &[Statement], state: &mut ParseState) -> Result<JboProp, String> {
    let mut props = Vec::new();
    for stmt in stmts {
        props.push(parse_statement(stmt, state)?);
    }
    Ok(crate::logic::big_and(props))
}

// Ported from: JboParse.hs :: parseStatement (lines 119-124)
/// Parse a statement with frees and prenex
/// Original Haskell:
///   parseStatement :: Statement -> JboPropM JboProp
///   parseStatement (Statement fs pts s) = do
///       doFreesInParseM (filter (not . isDiscursiveFree) fs)
///       parsePrenexTerms pts
///       parseStatement1 s
fn parse_statement(stmt: &Statement, state: &mut ParseState) -> Result<JboProp, String> {
    let old_var_bindings = std::mem::take(&mut state.var_bindings);
    let old_rvar_bindings = std::mem::take(&mut state.rvar_bindings);

    let result = (|| {
    // Filter out discursive frees (SEI/ti'o) - they attach to terms, not bridi
    let non_discursive: Vec<Free> = stmt
        .frees
        .iter()
        .filter(|f| !is_discursive_free(f))
        .cloned()
        .collect();

    do_frees_in_parse_m(&non_discursive, state)?;

    // Create BridiParseState for prenex terms
    let mut bridi_state = crate::parse_m::BridiParseState::new();

    // Parse prenex terms
    parse_prenex_terms(&stmt.prenex, &mut bridi_state, state)?;

    // Parse the statement body
    parse_statement1(&stmt.body, state)
    })();

    state.var_bindings = old_var_bindings;
    state.rvar_bindings = old_rvar_bindings;
    result
}

// Ported from: JboParse.hs :: parseStatement1 (lines 126-142)
/// Parse statement1 (handles connectives, paras, sentences)
/// Original Haskell:
///   parseStatement1 :: Statement1 -> JboPropM JboProp
///   parseStatement1 (ConnectedStatement con s1 s2) =
///       doConnective False con (parseStatement1 s1) (parseStatement1 s2)
///   parseStatement1 (StatementParas mtag ts) = do
///       traverse ((flip doTag Nothing =<<) . parseTag) mtag
///       pts <- mapM parseTexticule $ concat ts
///       return $ bigAnd $ rights pts
///   parseStatement1 (StatementSentence fs s) = do
///       doFreesInParseM (filter (not . isDiscursiveFree) fs)
///       b <- partiallyRunBridiM $ parseSentence s
///       modifyBribastiBindings $ setShunting (TUGOhA "go'i") b
///       return $ b nullArgs
fn parse_statement1(stmt1: &Statement1, state: &mut ParseState) -> Result<JboProp, String> {
    match stmt1 {
        Statement1::ConnectedStatement(conn, s1, s2) => {
            do_connective_prop(
                false,
                conn,
                |state| parse_statement1(s1, state),
                |state| parse_statement1(s2, state),
                state,
            )
        }
        Statement1::StatementParas { tag, paras } => {
            let jbo_tag = match tag {
                Some(tag) => Some(parse_tag_helper(tag, state)?),
                None => None,
            };

            let mut props = Vec::new();
            for para in paras {
                for texticule in para {
                    match texticule {
                        crate::jbo_syntax::FragmentOrStatement::Fragment(_frag) => {}
                        crate::jbo_syntax::FragmentOrStatement::Statement(st) => {
                            props.push(parse_statement(st, state)?);
                        }
                    }
                }
            }
            let prop = crate::logic::big_and(props);
            if let Some(jbo_tag) = jbo_tag {
                Ok(crate::logic::Prop::Modal(
                    crate::jbo_prop::JboModalOp::Tagged(jbo_tag, None),
                    Box::new(prop),
                ))
            } else {
                Ok(prop)
            }
        }
        Statement1::StatementSentence { frees, sentence } => {
            // Filter out discursive frees
            let non_discursive: Vec<Free> = frees
                .iter()
                .filter(|f| !is_discursive_free(f))
                .cloned()
                .collect();

            do_frees_in_parse_m(&non_discursive, state)?;

            // Create BridiParseState for this sentence
            let mut bridi_state = crate::parse_m::BridiParseState::new();

            let bridi = parse_sentence(sentence, &mut bridi_state, state)?;
            let captured_args = bridi_state.arglist.args.clone();
            let prop = bridi(&captured_args);
            let stored_bridi: crate::parse_m::StoredBridi = std::sync::Arc::new(move |args| {
                let combined_args = crate::parse_m::join_args(args, &captured_args);
                bridi(&combined_args)
            });
            state.bribasti_bindings = crate::parse_m::set_shunting(
                &|n| TanruUnit::TUGOhA("go'i".to_string(), n as i32),
                stored_bridi,
                &state.bribasti_bindings,
            );

            Ok(prop)
        }
    }
}

// Ported from: JboParse.hs :: parseTerms, parsePrenexTerms (lines 317-331)
/// Parse a list of terms
/// Original Haskell:
///   parseTerms,parsePrenexTerms :: PreProp r => [Term] -> ParseM r [JboTerm]
///   parseTerms = parseTerms' False
///   parsePrenexTerms = ignoringArgs . parseTerms' True
fn parse_terms(terms: &[Term], bridi_state: &mut crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Vec<JboTerm>, String> {
    parse_terms_helper(terms, false, bridi_state, state)
}

// Ported from: JboParse.hs :: parseTerms (prenex term parsing)
fn parse_prenex_terms(terms: &[Term], bridi_state: &mut crate::parse_m::BridiParseState, state: &mut ParseState) -> Result<Vec<JboTerm>, String> {
    let saved_arglist = bridi_state.arglist.clone();
    let result = parse_terms_helper(terms, true, bridi_state, state);
    bridi_state.arglist = saved_arglist;
    result
}

// Ported from: JboParse.hs :: parseTerms (term parsing with mode flag)
fn parse_terms_helper(
    terms: &[Term],
    prenex: bool,
    bridi_state: &mut crate::parse_m::BridiParseState,
    state: &mut ParseState,
) -> Result<Vec<JboTerm>, String> {
    let mut result = Vec::new();

    for term in terms {
        // Ported from JboParse.hs :: parseTerm: the "quantified buha in prenex" special case.
        if prenex {
            if let Term::Sumti(Tagged::Untagged, sumti) = term {
                if let Sumti::QSelbri { quant, selbri, .. } = sumti {
                    if let Selbri::Selbri2(sb2) = selbri {
                        if let Selbri2::Selbri3(sb3) = sb2 {
                            if let Selbri3::TanruHead(_, tu, _) = sb3 {
                                if tu_is_buha(tu) {
                                    // Parse quantifier and do buha
                                    let m = parse_mex(quant, state)?;
                                    do_buha(&m, tu, state)?;
                                    continue; // Skip adding to result
                                }
                            }
                        }
                    }
                }
            }
        }

        // Parse term normally
        let rets = parse_term(term, bridi_state, state)?;
        for ret in rets {
            match ret {
                Either::Left((jbo_tag, mo)) => {
                    do_tag_helper(&jbo_tag, mo, state)?;
                }
                Either::Right(o) => result.push(o),
            }
        }
    }

    Ok(result)
}

// Helper enum for parseTerm return type
enum Either<L, R> {
    Left(L),
    Right(R),
}

// Ported from: JboParse.hs :: parseTerm (lines 333-352)
/// Parse a single term
/// Original Haskell:
///   parseTerm :: PreProp r => Term -> ParseM r [Either (JboTag, Maybe JboTerm) JboTerm]
///   parseTerm t = case t of
///       Termset ts -> concat <$> mapM parseTerm ts
///       ConnectedTerms fore con t1 t2 -> doConnective fore con (parseTerm t1) (parseTerm t2)
///       Negation -> mapProp Not >> return []
///       Sumti (Tagged tag) s -> do
///           jbotag <- parseTag tag
///           o <- parseSumti s
///           return [Left (jbotag, Just o)]
///       Sumti taggedness s -> do
///           o <- parseSumti s
///           addArg $ Arg taggedness o
///           return [Right o]
///       BareFA (Just n) -> setArgPos n >> return []
///       BareFA Nothing -> return []
///       NullTerm -> return []
///       BareTag tag -> (\jt -> [Left (jt, Nothing)]) <$> parseTag tag
fn parse_term(
    term: &Term,
    bridi_state: &mut crate::parse_m::BridiParseState,
    state: &mut ParseState,
) -> Result<Vec<Either<(crate::jbo_prop::JboTag, Option<JboTerm>), JboTerm>>, String> {
    match term {
        Term::Termset(ts) => {
            // Flatten termset
            let mut result = Vec::new();
            for t in ts {
                result.extend(parse_term(t, bridi_state, state)?);
            }
            Ok(result)
        }
        Term::ConnectedTerms(forethought, conn, term1, term2) => {
            let mut outer_transforms = std::mem::take(&mut state.pending_prop_transforms);
            let mut left_bridi_state = bridi_state.clone();
            let mut left_state = state.clone();
            let mut right_bridi_state = bridi_state.clone();
            let mut right_state = state.clone();

            let mtag = match conn {
                Connective::JboConnLog(mtag, _) => mtag.as_ref(),
                Connective::JboConnJoik(mtag, _) => mtag.as_ref(),
            };
            let outer_transform_start = state.pending_prop_transforms.len();

            if let Some(tag) = mtag {
                let v = quantify_helper(
                    &crate::jbo_syntax::Mex::MexNumeralString(vec![
                        crate::jbo_syntax::Numeral::PA("su'o".to_string())
                    ]),
                    None,
                    state,
                )?;
                outer_transforms.extend(state.pending_prop_transforms.split_off(outer_transform_start));

                left_state.next_fresh_var = state.next_fresh_var;
                left_state.variable_domains = state.variable_domains.clone();
                right_state.next_fresh_var = state.next_fresh_var;
                right_state.variable_domains = state.variable_domains.clone();

                if *forethought {
                    let jtag = parse_tag_helper(tag, state)?;
                    do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), &mut left_state);
                    do_tag_helper(&jtag, Some(v), &mut right_state)?;
                } else if is_tense_tag(tag) {
                    do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v.clone()), &mut left_state);
                    let jtag = parse_tag_helper(tag, state)?;
                    do_tag_helper(&jtag, Some(v), &mut right_state)?;
                } else {
                    let jtag = parse_tag_helper(tag, state)?;
                    do_tag_helper(&jtag, Some(v.clone()), &mut left_state)?;
                    do_modal_helper(&crate::jbo_prop::JboModalOp::WithEventAs(v), &mut right_state);
                }
            }

            let result = parse_term(term1, &mut left_bridi_state, &mut left_state)?;
            for ret in result {
                if let Either::Left((jbo_tag, mo)) = ret {
                    do_tag_helper(&jbo_tag, mo, &mut left_state)?;
                }
            }
            let left_arglist = left_bridi_state.arglist.clone();
            right_state.sumbasti_bindings = left_state.sumbasti_bindings.clone();
            right_state.backcount_stack = left_state.backcount_stack.clone();
            right_state.bribasti_bindings = left_state.bribasti_bindings.clone();
            right_state.next_fresh_var = left_state.next_fresh_var;
            right_state.next_fresh_rvar = left_state.next_fresh_rvar;
            right_state.variable_domains = left_state.variable_domains.clone();
            right_state.next_fresh_constant = left_state.next_fresh_constant;
            right_state.referenced_vars = left_state.referenced_vars.clone();
            right_state.questions = left_state.questions.clone();
            right_state.lambdas = left_state.lambdas.clone();
            right_state.side_texticules = left_state.side_texticules.clone();
            right_state.non_veridical_props = left_state.non_veridical_props.clone();
            let right_result = parse_term(term2, &mut right_bridi_state, &mut right_state)?;
            for ret in right_result {
                if let Either::Left((jbo_tag, mo)) = ret {
                    do_tag_helper(&jbo_tag, mo, &mut right_state)?;
                }
            }
            let right_arglist = right_bridi_state.arglist.clone();

            let left_transforms = left_state.pending_prop_transforms.drain(..).collect::<Vec<_>>();
            let right_transforms = right_state.pending_prop_transforms.drain(..).collect::<Vec<_>>();
            let conn = conn.clone();
            let transforms = outer_transforms;

            *bridi_state = right_bridi_state;
            *state = right_state;
            state.map_prop(crate::parse_m::PropTransform::ConnectBranches {
                conn,
                left_args: left_arglist.args.clone(),
                right_args: right_arglist.args.clone(),
                continuation_position: right_arglist.position,
                left_transforms,
                right_transforms,
                outer_transforms: transforms,
            });
            bridi_state.arglist = right_arglist;
            for (pos, term) in left_arglist.args {
                bridi_state.arglist.args.insert(pos, term);
            }
            Ok(vec![])
        }
        Term::Negation => {
            // Negation - apply to proposition
            state.map_prop(crate::parse_m::PropTransform::Negate);
            Ok(vec![])
        }
        Term::Sumti(taggedness, sumti) => {
            let o = parse_sumti(sumti, state)?;
            match taggedness {
                Tagged::Tagged(tag) => {
                    // Tagged sumti
                    let jbo_tag = parse_tag_helper(tag, state)?;
                    Ok(vec![Either::Left((jbo_tag, Some(o)))])
                }
                Tagged::Untagged => {
                    // Untagged sumti - add as argument
                    // Ported from: JboParse.hs :: parseTerm (line 351)
                    //   Sumti taggedness s -> do
                    //       o <- parseSumti s
                    //       addArg $ Arg taggedness o
                    //       return [Right o]
                    crate::parse_m::add_arg(crate::parse_m::Arg::Untagged(o.clone()), bridi_state, state);
                    Ok(vec![Either::Right(o)])
                }
                Tagged::FATagged(n) => {
                    crate::parse_m::add_arg(crate::parse_m::Arg::FATagged(*n as usize, o.clone()), bridi_state, state);
                    Ok(vec![Either::Right(o)])
                }
                Tagged::FAITagged => {
                    crate::parse_m::add_arg(crate::parse_m::Arg::FAITagged(o.clone()), bridi_state, state);
                    Ok(vec![Either::Right(o)])
                }
            }
        }
        Term::BareFA(n) => {
            // Bare FA - set argument position
            if let Some(pos) = n {
                crate::parse_m::set_arg_pos(*pos, bridi_state);
            }
            Ok(vec![])
        }
        Term::NullTerm => Ok(vec![]),
        Term::BareTag(tag) => {
            // Bare tag
            let jbo_tag = parse_tag_helper(tag, state)?;
            Ok(vec![Either::Left((jbo_tag, None))])
        }
    }
}

// Ported from: JboParse.hs :: parseVariable (lines 494-502)
/// Parse a variable sumti with quantification
/// Original Haskell:
///   parseVariable :: PreProp r => SumtiAtom -> [JboPred] -> Maybe JboMex -> ParseM r JboTerm
///   parseVariable sa@(Variable n) rps mm = do
///       x <- lookupVarBinding sa
///       case (x,mm) of
///            (Just o,Nothing) -> return o
///            _ -> do
///               o <- quantify (fromMaybe mexExists mm) $ andMPred rps
///               setVarBinding sa o
///               return o
fn parse_variable(
    sa: &SumtiAtom,
    rps: &[JboPred],
    mm: Option<&JboMex>,
    state: &mut ParseState,
) -> Result<JboTerm, String> {
    let existing = state.var_bindings.get(sa).cloned();

    match (existing, mm) {
        (Some(o), None) => {
            // Already bound, no quantifier - return existing
            Ok(o)
        }
        _ => {
            // Not bound or has quantifier - quantify and bind
            let m = mm.cloned().unwrap_or_else(|| {
                JboMex::MexNumeralString(vec![crate::jbo_syntax::Numeral::PA("su'o".to_string())])
            });

            let restrict = crate::jbo_prop::and_m_pred(rps.to_vec());
            let o = quantify(&m, restrict, state)?;
            state.var_bindings.insert(sa.clone(), o.clone());
            Ok(o)
        }
    }
}

// Ported from: JboParse.hs :: doBuha (lines 303-314)
/// Handle bu'a/bu'e/bu'i variable selbri (quantification over relations)
/// Original Haskell:
///   doBuha :: PreProp r => JboMex -> TanruUnit -> ParseM r JboRel
///   doBuha m tu@(TUGOhA g n) | tuIsBuha tu =
///       let bn = if n > 1 then n else head [n | (v,n) <- zip "aei" [1..], g=="bu'"++[v]]
///           tu = TUGOhA "bu'a" bn
///       in do
///           x <- lookupVarBinding tu
///           case x of
///               Just (RVar n) -> return $ RVar n
///               Nothing -> do
///                   r <- rquantify m
///                   setVarBinding tu r
///                   return r
fn do_buha(
    m: &JboMex,
    tu: &TanruUnit,
    state: &mut ParseState,
) -> Result<JboRel, String> {
    if !tu_is_buha(tu) {
        return Err("Not a buha tanru unit".to_string());
    }

    // Extract bu'a/bu'e/bu'i and number
    let (base, n) = match tu {
        TanruUnit::TUGOhA(goha, number) => {
            let num = if *number > 1 {
                *number
            } else {
                // Map bu'a->1, bu'e->2, bu'i->3
                if goha == "bu'a" {
                    1
                } else if goha == "bu'e" {
                    2
                } else if goha == "bu'i" {
                    3
                } else {
                    1
                }
            };
            ("bu'a", num)
        }
        _ => return Err("Invalid buha".to_string()),
    };

    // Create canonical TUGOhA for lookup
    let canonical_tu = TanruUnit::TUGOhA(base.to_string(), n);

    let existing = state.rvar_bindings.get(&canonical_tu).cloned();

    match existing {
        Some(r) => Ok(r),
        None => {
            let r = rquantify(m, state)?;
            state.rvar_bindings.insert(canonical_tu, r.clone());
            Ok(r)
        }
    }
}

// Ported from: JboParse.hs :: parsedSelbriToNewSelbri (lines 720-721)
/// Convert parsed selbri to new selbri scope
/// Original Haskell:
///   parsedSelbriToNewSelbri :: BridiM Bridi -> ParseM r Bridi
///   parsedSelbriToNewSelbri m = closeBridi <$> partiallyRunSubBridiM m
fn parsed_selbri_to_new_selbri<F>(
    bridi_state: &crate::parse_m::BridiParseState,
    state: &mut ParseState,
    f: F,
) -> Result<Bridi, String>
where
    F: FnOnce(&mut crate::parse_m::BridiParseState, &mut ParseState) -> Result<Bridi, String>,
{
    let bridi = parse_seltau_bridi(bridi_state, state, f)?;
    Ok(crate::parse_m::close_bridi(bridi))
}

// Helper: Swap argument positions in a proposition
// Ported from: JboParse.hs :: parsedSelbriToNewSelbri (swap argument positions in proposition)
fn swap_arg_positions_in_prop(n: i32, prop: crate::parse_m::JboProp) -> crate::parse_m::JboProp {
    use crate::logic::Prop;

    match prop {
        Prop::Rel(rel, terms) => {
            // Swap arguments in the term list
            let mut new_terms = terms.clone();
            if n >= 1 && n <= 4 {
                let idx = n as usize; // Position to swap with x1 (position 0)
                if new_terms.len() > idx {
                    new_terms.swap(0, idx);
                }
            }
            Prop::Rel(rel, new_terms)
        }
        Prop::Not(p) => Prop::Not(Box::new(swap_arg_positions_in_prop(n, *p))),
        Prop::Connected(c, p1, p2) => Prop::Connected(
            c,
            Box::new(swap_arg_positions_in_prop(n, *p1)),
            Box::new(swap_arg_positions_in_prop(n, *p2)),
        ),
        Prop::NonLogConnected(c, p1, p2) => Prop::NonLogConnected(
            c,
            Box::new(swap_arg_positions_in_prop(n, *p1)),
            Box::new(swap_arg_positions_in_prop(n, *p2)),
        ),
        Prop::Modal(m, p) => Prop::Modal(m, Box::new(swap_arg_positions_in_prop(n, *p))),
        Prop::Quantified(q, r, f) => {
            // Apply permutation through quantifier
            Prop::Quantified(q, r, std::rc::Rc::new(move |v| swap_arg_positions_in_prop(n, f(v))))
        }
        other => other, // Eet, Oot unchanged
    }
}

// Ported from: JboShow.hs :: jboshow for propositions
/// Build Lojban output from proposition and non-veridical predicates
fn push_canonical_lines(lines: &mut Vec<String>, lojban: &str) {
    for line in lojban.lines().filter(|line| !line.is_empty()) {
        if lines.is_empty() || line.starts_with(".i ") {
            lines.push(line.to_string());
        } else {
            lines.push(format!(".i {}", line));
        }
    }
}

// Ported from: JboShow.hs :: propToLojban (canonical line formatting)
fn build_lojban_output_from_prop(
    prop: &crate::parse_m::JboProp,
    side_props: &[JboProp],
) -> String {
    use crate::jbo_show::prop_to_lojban;

    let mut lines = Vec::new();

    for side_prop in side_props {
        push_canonical_lines(&mut lines, &prop_to_lojban(side_prop));
    }

    push_canonical_lines(&mut lines, &prop_to_lojban(prop));

    lines.join("\n")
}

// Ported from: JboShow.hs :: propToLojban (texticule-based output formatting)
fn build_lojban_output_from_texticules(
    prop: &crate::parse_m::JboProp,
    side_texticules: &[Texticule],
) -> String {
    use crate::jbo_show::prop_to_lojban;

    let mut lines = Vec::new();

    for side_texticule in side_texticules {
        if let Ok(parse_texticule) = prop_texticule_to_parse(side_texticule.clone()) {
            push_canonical_lines(&mut lines, &prop_to_lojban(&prop_texticule_to_prop(parse_texticule)));
        }
    }

    push_canonical_lines(&mut lines, &prop_to_lojban(prop));

    lines.join("\n")
}
