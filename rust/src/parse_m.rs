//! Parse state monad for semantic analysis
//!
//! Ported from: ParseM.hs
//!
//! This module provides the monadic context for parsing and semantic analysis.
//! It manages:
//! - Fresh variable/constant generation
//! - Variable binding context
//! - Sumbasti/bribasti bindings (anaphora resolution)
//! - Question handling
//! - Lambda variable tracking
//! - Argument list management

use crate::jbo_prop::{JboRel, JboTerm, JboQuantifier, JboTag, JboModalOp, JboVPred, JboNPred, JboPred, SideType};
use std::sync::Arc;
use crate::jbo_syntax::{SumtiAtom, SumtiQualifier, TanruUnit, Lerfu, Selbri};
use crate::logic::Prop;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

pub type JboProp = Prop<JboRel, JboTerm, String, crate::jbo_prop::JboModalOp, JboQuantifier>;

// Ported from: ParseM.hs :: type Bridi = Args -> JboProp
/// A bridi is a function from arguments to a proposition
pub type Bridi = Box<dyn Fn(&Args) -> JboProp + Send + Sync>;

// Ported from: ParseM.hs :: ParseState
/// ParseState holds all the state which doesn't respect the tree structure
/// of the logical parse; it is threaded through as we parse the text left-to-right.
#[derive(Clone)]
pub struct ParseState {
    pub sumbasti_bindings: SumbastiBindings,
    pub bribasti_bindings: BribastiBindings,
    pub backcount_stack: Vec<i32>,
    pub next_fresh_var: i32,
    pub next_fresh_rvar: i32,
    pub variable_domains: HashMap<i32, VariableDomain>,
    pub next_fresh_constant: i32,
    pub referenced_vars: HashSet<i32>,
    pub questions: Vec<Question>,
    pub lambdas: HashMap<LambdaPos, i32>,
    pub side_texticules: Vec<Texticule>,
    pub pending_prop_transforms: Vec<PropTransform>,
    pub non_veridical_props: Vec<JboProp>,
    pub var_bindings: VarBindings,
    pub rvar_bindings: RVarBindings,
}

impl std::fmt::Debug for ParseState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseState")
            .field("sumbasti_bindings", &self.sumbasti_bindings)
            .field("bribasti_bindings_len", &self.bribasti_bindings.len())
            .field("backcount_stack", &self.backcount_stack)
            .field("next_fresh_var", &self.next_fresh_var)
            .field("next_fresh_rvar", &self.next_fresh_rvar)
            .field("variable_domains", &self.variable_domains)
            .field("next_fresh_constant", &self.next_fresh_constant)
            .field("referenced_vars", &self.referenced_vars)
            .field("questions", &self.questions)
            .field("lambdas", &self.lambdas)
            .field("side_texticules", &self.side_texticules)
            .field("pending_prop_transforms", &self.pending_prop_transforms)
            .field("non_veridical_props", &self.non_veridical_props)
            .field("var_bindings", &self.var_bindings)
            .field("rvar_bindings", &self.rvar_bindings)
            .finish()
    }
}

/// Represents a transformation to apply to a proposition (like quantification, negation, modals)
#[derive(Clone)]
#[allow(clippy::large_enum_variant)]
pub enum PropTransform {
    Quantify(JboTerm, JboQuantifier, Option<Arc<dyn Fn(i32) -> JboProp + Send + Sync>>),
    ConnectQuantify {
        conn: crate::jbo_syntax::LogJboConnective,
        fresh: JboTerm,
        left_quant: JboQuantifier,
        right_quant: JboQuantifier,
        restriction: Option<Arc<dyn Fn(i32) -> JboProp + Send + Sync>>,
    },
    ConnectQuantifyTagged {
        conn: crate::jbo_syntax::LogJboConnective,
        fresh: JboTerm,
        left_quant: JboQuantifier,
        right_quant: JboQuantifier,
        restriction: Option<Arc<dyn Fn(i32) -> JboProp + Send + Sync>>,
        event_fresh: Option<JboTerm>,
        left_modal: Option<JboModalOp>,
        right_modal: Option<JboModalOp>,
    },
    RQuantify(JboRel, JboQuantifier),
    SubstituteTerm(JboTerm, JboTerm),
    LogicalConnectBranches {
        conn: crate::jbo_syntax::LogJboConnective,
        fresh: JboTerm,
        left_term: JboTerm,
        left_transforms: Vec<PropTransform>,
        right_term: JboTerm,
        right_transforms: Vec<PropTransform>,
    },
    ConnectBranches {
        conn: crate::jbo_syntax::Connective,
        left_args: Args,
        right_args: Args,
        continuation_position: i32,
        left_transforms: Vec<PropTransform>,
        right_transforms: Vec<PropTransform>,
        outer_transforms: Vec<PropTransform>,
    },
    ConnectTagBranches {
        conn: crate::jbo_prop::JboConnective,
        left_transforms: Vec<PropTransform>,
        right_transforms: Vec<PropTransform>,
    },
    Negate,
    ApplyModal(JboModalOp),
}

impl Default for ParseState {
    fn default() -> Self {
        Self::new()
    }
}

impl ParseState {
    pub fn new() -> Self {
        ParseState {
            sumbasti_bindings: HashMap::new(),
            bribasti_bindings: HashMap::new(),
            backcount_stack: Vec::new(),
            next_fresh_var: 0,
            next_fresh_rvar: 0,
            variable_domains: HashMap::new(),
            next_fresh_constant: 0,
            referenced_vars: HashSet::new(),
            questions: Vec::new(),
            lambdas: HashMap::new(),
            side_texticules: Vec::new(),
            pending_prop_transforms: Vec::new(),
            non_veridical_props: Vec::new(),
            var_bindings: HashMap::new(),
            rvar_bindings: HashMap::new(),
        }
    }

    /// Add a pending transformation to be applied to the next proposition
    pub fn map_prop(&mut self, transform: PropTransform) {
        self.pending_prop_transforms.push(transform);
    }

    /// Apply all pending transformations to a proposition and clear the list
    pub fn apply_transforms(&mut self, mut prop: JboProp) -> JboProp {
        for transform in self.pending_prop_transforms.drain(..).rev() {
            prop = apply_transform(transform, prop);
        }
        prop
    }

    // Ported from: ParseM.hs :: getFreshVar
    pub fn get_fresh_var(&mut self, dom: VariableDomain) -> JboTerm {
        let n = self.next_fresh_var;
        self.next_fresh_var += 1;
        self.variable_domains.insert(n, dom);
        JboTerm::Var(n)
    }

    // Ported from: ParseM.hs :: getFreshConstant
    pub fn get_fresh_constant(&mut self) -> JboTerm {
        let n = self.next_fresh_constant;
        self.next_fresh_constant += 1;
        JboTerm::Constant(n, Vec::new())
    }

    // Ported from: ParseM.hs :: getFreshRVar
    pub fn get_fresh_rvar(&mut self) -> JboRel {
        let n = self.next_fresh_rvar;
        self.next_fresh_rvar += 1;
        JboRel::RVar(n)
    }

    // Ported from: ParseM.hs :: getSumbasti
    /// Get sumbasti binding for a sumti atom
    pub fn get_sumbasti(&self, atom: &SumtiAtom) -> JboTerm {
        get_sumbasti_binding(&self.sumbasti_bindings, atom)
    }

    // Ported from: ParseM.hs :: setSumbasti
    /// Set sumbasti binding
    pub fn set_sumbasti(&mut self, sumbasti: Sumbasti, term: JboTerm) {
        self.sumbasti_bindings.insert(sumbasti, term);
    }

    pub fn update_ri(&mut self, o: &JboTerm) {
        let mut new_bindings = HashMap::new();
        for (key, val) in self.sumbasti_bindings.iter() {
            if let SumtiAtom::Ri(n) = &key.atom {
                let new_key = Sumbasti {
                    explicitly_assigned: key.explicitly_assigned,
                    atom: SumtiAtom::Ri(n + 1),
                };
                new_bindings.insert(new_key, val.clone());
            } else {
                new_bindings.insert(key.clone(), val.clone());
            }
        }
        self.sumbasti_bindings = new_bindings;
        let count = self.backcount_stack.first().copied().unwrap_or(0);
        let sumbasti = Sumbasti {
            explicitly_assigned: false,
            atom: SumtiAtom::Ri(count),
        };
        self.set_sumbasti(sumbasti, o.clone());
    }

    // Ported from: ParseM.hs :: updateSumbastiWithSumtiAtom
    /// Update sumbasti bindings based on sumti atom
    pub fn update_sumbasti_with_sumti_atom(&mut self, sa: &SumtiAtom, o: &JboTerm) {
        if crate::jbo_parse::gets_ri(sa) {
            let count = self.pop_backcount();
            self.sumbasti_bindings = set_shunting(
                &|n| Sumbasti {
                    explicitly_assigned: false,
                    atom: SumtiAtom::Ri(count + n as i32),
                },
                o.clone(),
                &self.sumbasti_bindings,
            );
        }

        // Handle name first-letter binding
        if let SumtiAtom::Name(_, _, name) = sa {
            if let Some(first_char) = name.chars().next() {
                let lerfu = SumtiAtom::LerfuString(vec![Lerfu::LerfuChar(first_char)]);
                let sumbasti = Sumbasti {
                    explicitly_assigned: false,
                    atom: lerfu,
                };
                self.set_sumbasti(sumbasti, o.clone());
            }
        }

        if let SumtiAtom::Description(_, _, _, crate::jbo_syntax::EitherSelbriSumti::Selbri(sb), _, _) = sa {
            for lerfu in crate::jbo_syntax::lerfu_strings_of_selbri(sb) {
                let sumbasti = Sumbasti {
                    explicitly_assigned: false,
                    atom: SumtiAtom::LerfuString(lerfu),
                };
                self.set_sumbasti(sumbasti, o.clone());
            }
        }
    }

    // Ported from: ParseM.hs :: pushBackcount
    /// Push a new backcount level for ri/ra/ru
    pub fn push_backcount(&mut self) {
        let new_stack: Vec<i32> = std::iter::once(0)
            .chain(self.backcount_stack.iter().map(|n| n + 1))
            .collect();
        self.backcount_stack = new_stack;
    }

    // Ported from: ParseM.hs :: popBackcount
    /// Pop backcount and return the count
    pub fn pop_backcount(&mut self) -> i32 {
        if self.backcount_stack.is_empty() {
            return 0;
        }
        self.backcount_stack.remove(0)
    }

    // Ported from: ParseM.hs :: setReferenced
    /// Mark a variable as referenced
    pub fn set_referenced(&mut self, n: i32) {
        self.referenced_vars.insert(n);
    }

    // Ported from: ParseM.hs :: referenced
    /// Check if a variable is referenced
    pub fn referenced(&self, n: i32) -> bool {
        self.referenced_vars.contains(&n)
    }

    // Ported from: ParseM.hs :: putLambda
    /// Put a lambda variable and return the term
    pub fn put_lambda(&mut self, mlev: Option<i32>, mnum: Option<i32>) -> JboTerm {
        let lev = mlev.unwrap_or(1);
        let next = self.lambdas.keys().filter_map(|pos| if pos.level == lev { Some(pos.num) } else { None })
            .max()
            .unwrap_or(0) + 1;
        let num = mnum.unwrap_or(next);
        let pos = LambdaPos { level: lev, num };

        if let Some(&n) = self.lambdas.get(&pos) {
            return JboTerm::Var(n);
        }

        let fresh = self.get_fresh_var(VariableDomain::UnrestrictedDomain);
        if let JboTerm::Var(n) = fresh {
            self.lambdas.insert(pos, n);
            JboTerm::Var(n)
        } else {
            fresh
        }
    }
}

// Ported from: ParseM.hs :: BridiParseState
/// BridiParseState holds state which respects the logical structure
#[derive(Debug, Clone)]
pub struct BridiParseState {
    pub arglist: Arglist,
    pub var_bindings: VarBindings,
    pub rvar_bindings: RVarBindings,
    pub is_sub_bridi: bool,
}

impl Default for BridiParseState {
    fn default() -> Self {
        Self::new()
    }
}

impl BridiParseState {
    pub fn new() -> Self {
        BridiParseState {
            arglist: Arglist::new(),
            var_bindings: HashMap::new(),
            rvar_bindings: HashMap::new(),
            is_sub_bridi: false,
        }
    }
}

// Ported from: ParseM.hs :: Sumbasti
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Sumbasti {
    pub explicitly_assigned: bool,
    pub atom: SumtiAtom,
}

pub type SumbastiBindings = HashMap<Sumbasti, JboTerm>;
pub type StoredBridi = Arc<dyn Fn(&Args) -> JboProp + Send + Sync>;
pub type BribastiBindings = HashMap<TanruUnit, StoredBridi>;

// Ported from: ParseM.hs :: Question
#[derive(Debug, Clone)]
pub struct Question {
    pub kau_depth: Option<i32>,
    pub info: QInfo,
}

// Ported from: ParseM.hs :: QInfo
pub enum QInfo {
    QTruth,
    QSumti(JboTerm, Option<JboPred>),
    QBridi(JboRel),
}

impl Clone for QInfo {
    fn clone(&self) -> Self {
        match self {
            QInfo::QTruth => QInfo::QTruth,
            QInfo::QSumti(term, dom) => QInfo::QSumti(term.clone(), dom.clone()),
            QInfo::QBridi(rel) => QInfo::QBridi(rel.clone()),
        }
    }
}

impl std::fmt::Debug for QInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QInfo::QTruth => write!(f, "QTruth"),
            QInfo::QSumti(term, _) => write!(f, "QSumti({:?})", term),
            QInfo::QBridi(rel) => write!(f, "QBridi({:?})", rel),
        }
    }
}

// Ported from: ParseM.hs :: LambdaPos
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LambdaPos {
    pub level: i32,
    pub num: i32,
}

// Ported from: ParseM.hs :: VariableDomain
pub enum VariableDomain {
    UnrestrictedDomain,
    PredDomain(JboPred),
    FiniteDomain(Vec<JboTerm>),
}

impl Clone for VariableDomain {
    fn clone(&self) -> Self {
        match self {
            VariableDomain::UnrestrictedDomain => VariableDomain::UnrestrictedDomain,
            VariableDomain::PredDomain(pred) => VariableDomain::PredDomain(pred.clone()),
            VariableDomain::FiniteDomain(os) => VariableDomain::FiniteDomain(os.clone()),
        }
    }
}

impl std::fmt::Debug for VariableDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableDomain::UnrestrictedDomain => write!(f, "UnrestrictedDomain"),
            VariableDomain::PredDomain(_) => write!(f, "PredDomain(<fn>)"),
            VariableDomain::FiniteDomain(os) => f.debug_tuple("FiniteDomain").field(os).finish(),
        }
    }
}

// Ported from: ParseM.hs :: Arglist
#[derive(Debug, Clone)]
pub struct Arglist {
    pub args: Args,
    pub position: i32,
}

impl Default for Arglist {
    fn default() -> Self {
        Self::new()
    }
}

impl Arglist {
    pub fn new() -> Self {
        Arglist {
            args: HashMap::new(),
            position: 1,
        }
    }
}

pub type Args = HashMap<ArgPos, JboTerm>;

// Ported from: ParseM.hs :: ArgPos
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ArgPos {
    NPos(i32),
    JaiPos,
    UnfilledPos(i32),
}

pub type VarBindings = HashMap<SumtiAtom, JboTerm>;
pub type RVarBindings = HashMap<TanruUnit, JboRel>;

// Ported from: ParseM.hs :: Texticule
#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum Texticule {
    TexticuleProp(JboProp),
    TexticuleSide(SideType, Box<Texticule>),
}

// Ported from: ParseM.hs :: argsToJboterms
pub fn args_to_jboterms(args: &Args) -> Vec<JboTerm> {
    let max_n = args.keys()
        .filter_map(|k| match k {
            ArgPos::NPos(n) => Some(*n),
            _ => None,
        })
        .max()
        .unwrap_or(0);

    (1..=max_n)
        .map(|n| args.get(&ArgPos::NPos(n)).cloned().unwrap_or(JboTerm::Unfilled))
        .collect()
}

// ============================================================================
// Additional functions from ParseM.hs
// ============================================================================

// Ported from: ParseM.hs :: jboRelToBridi
/// Convert a JboRel to a Bridi function
pub fn jbo_rel_to_bridi(rel: JboRel) -> Bridi {
    Box::new(move |args: &Args| {
        let terms = args_to_jboterms(args);
        Prop::Rel(rel.clone(), terms)
    })
}

// Apply pending transforms to a bridi, returning a new bridi that applies transforms when called
pub fn apply_transforms_to_bridi(bridi: Bridi, state: &mut ParseState) -> Bridi {
    let transforms = state.pending_prop_transforms.drain(..).collect::<Vec<_>>();
    apply_transform_list_to_bridi(bridi, transforms)
}

pub fn apply_transform_list_to_bridi(bridi: Bridi, transforms: Vec<PropTransform>) -> Bridi {
    if transforms.is_empty() {
        return bridi;
    }

    Box::new(move |args: &Args| {
        let mut prop = bridi(args);
        for transform in transforms.iter().rev() {
            prop = apply_transform(transform.clone(), prop);
        }
        prop
    })
}

// Ported from: ParseM.hs :: withJaiAsTag
/// Wrap a bridi with JAI as a modal tag
pub fn with_jai_as_tag(jtag: crate::jbo_prop::JboTag, bridi: Bridi) -> Bridi {
    Box::new(move |args: &Args| {
        let jai_term = args.get(&ArgPos::JaiPos).cloned();
        let mut new_args = args.clone();
        new_args.remove(&ArgPos::JaiPos);

        let inner_prop = bridi(&new_args);
        Prop::Modal(
            crate::jbo_prop::JboModalOp::Tagged(jtag.clone(), jai_term),
            Box::new(inner_prop)
        )
    })
}

// Ported from: ParseM.hs :: withJaiAsRaising
/// Wrap a bridi with JAI as raising (tu'a)
pub fn with_jai_as_raising(bridi: Bridi) -> Bridi {
    Box::new(move |args: &Args| {
        if let Some(jai_term) = args.get(&ArgPos::JaiPos) {
            let mut new_args = args.clone();
            new_args.remove(&ArgPos::JaiPos);

            let qualified = JboTerm::QualifiedTerm(
                SumtiQualifier::LAhE("tu'a".to_string()),
                Box::new(jai_term.clone()),
            );
            new_args.insert(ArgPos::NPos(1), qualified);

            bridi(&new_args)
        } else {
            bridi(args)
        }
    })
}

// Ported from: ParseM.hs :: partiallyResolveBridi
/// Partially resolve a bridi by capturing its arglist
/// Returns a new bridi that joins extra args with the captured arglist
pub fn partially_resolve_bridi(bridi: Bridi, arglist: &Args) -> Bridi {
    let captured_args = arglist.clone();
    Box::new(move |extra_args: &Args| {
        let joined = join_args(extra_args, &captured_args);
        bridi(&joined)
    })
}

// Ported from: ParseM.hs :: joinArgs
/// Join two argument maps, handling implicit arguments
pub fn join_args(new: &Args, old: &Args) -> Args {
    let mut result = old.clone();

    let mut old_positions: Vec<i32> = old.keys()
        .filter_map(|k| match k {
            ArgPos::NPos(n) => Some(*n),
            _ => None,
        })
        .collect();
    old_positions.sort();

    let mut implicit_args: Vec<(i32, JboTerm)> = new.iter()
        .filter_map(|(k, v)| match k {
            ArgPos::UnfilledPos(n) if !matches!(v, JboTerm::Unfilled) => Some((*n, v.clone())),
            _ => None,
        })
        .collect();
    implicit_args.sort_by_key(|(n, _)| *n);

    for (n, value) in implicit_args.into_iter().rev() {
        let mut gap = 1;
        let mut seen = 0;
        loop {
            if !old_positions.contains(&gap) {
                if seen == n {
                    result.insert(ArgPos::NPos(gap), value);
                    break;
                }
                seen += 1;
            }
            gap += 1;
        }
    }

    for (k, v) in new.iter() {
        if !matches!(k, ArgPos::UnfilledPos(_)) {
            result.insert(k.clone(), v.clone());
        }
    }

    result
}

// Ported from: ParseM.hs :: swapArgs
/// Swap two argument positions
pub fn swap_args(p1: &ArgPos, p2: &ArgPos, args: &Args) -> Args {
    args.iter()
        .map(|(k, v)| {
            let new_k = if k == p1 {
                p2.clone()
            } else if k == p2 {
                p1.clone()
            } else {
                k.clone()
            };
            (new_k, v.clone())
        })
        .collect()
}

// Ported from: ParseM.hs :: setArg
/// Set an argument at a specific position
pub fn set_arg(pos: ArgPos, term: JboTerm, args: &Args) -> Args {
    let mut result = args.clone();
    result.insert(pos, term);
    result
}

// Ported from: ParseM.hs :: addFaiToArglist
/// Add FAI term to arglist
pub fn add_fai_to_arglist(term: JboTerm, arglist: &Arglist) -> Arglist {
    let mut new_args = arglist.args.clone();
    new_args.insert(ArgPos::JaiPos, term);
    Arglist {
        args: new_args,
        position: arglist.position,
    }
}

// Ported from: ParseM.hs :: closeBridi
/// Close a bridi by converting filled places to unfilled positions
pub fn close_bridi(bridi: Bridi) -> Bridi {
    Box::new(move |args: &Args| {
        let terms = args_to_jboterms(args);
        let mut new_args: Args = terms.iter()
            .enumerate()
            .map(|(i, t)| (ArgPos::UnfilledPos(i as i32), t.clone()))
            .collect();
        
        // Preserve JAI position if present
        if let Some(jai_term) = args.get(&ArgPos::JaiPos) {
            new_args.insert(ArgPos::JaiPos, jai_term.clone());
        }
        
        bridi(&new_args)
    })
}

// Ported from: ParseM.hs :: swapTerms
/// Swap two terms in a list by position (1-indexed)
pub fn swap_terms(terms: &[JboTerm], n: i32, m: i32) -> Vec<JboTerm> {
    crate::util::swap_finite_with_default(JboTerm::Unfilled, terms, (n - 1) as usize, (m - 1) as usize)
}

impl ParseState {
    // Ported from: ParseM.hs :: addSideTexticule
    /// Add a side texticule to the parse state
    pub fn add_side_texticule(&mut self, texticule: Texticule) {
        self.side_texticules.insert(0, texticule);
    }
    
    // Ported from: ParseM.hs :: takeSideTexticules
    /// Take all side texticules and clear the list
    pub fn take_side_texticules(&mut self) -> Vec<Texticule> {
        std::mem::take(&mut self.side_texticules)
    }
    
    // Ported from: ParseM.hs :: addSumtiQuestion
    /// Add a sumti question (ma) to the parse state
    pub fn add_sumti_question(&mut self, kau_depth: Option<i32>) -> JboTerm {
        let var = self.get_fresh_var(VariableDomain::UnrestrictedDomain);
        self.questions.insert(0, Question {
            kau_depth,
            info: QInfo::QSumti(var.clone(), None),
        });
        var
    }
    
    // Ported from: ParseM.hs :: addBridiQuestion
    /// Add a bridi question (mo) to the parse state
    pub fn add_bridi_question(&mut self, kau_depth: Option<i32>) -> JboRel {
        let rvar = self.get_fresh_rvar();
        self.questions.insert(0, Question {
            kau_depth,
            info: QInfo::QBridi(rvar.clone()),
        });
        rvar
    }
    
    // Ported from: ParseM.hs :: addQuestion
    /// Add a question to the parse state
    pub fn add_question(&mut self, question: Question) {
        self.questions.insert(0, question);
    }
    
    // Ported from: ParseM.hs :: modifyQuestions
    /// Modify the questions list
    pub fn modify_questions<F>(&mut self, f: F)
    where
        F: FnOnce(Vec<Question>) -> Vec<Question>,
    {
        let questions = std::mem::take(&mut self.questions);
        self.questions = f(questions);
    }
}

impl BridiParseState {
    // Ported from: ParseM.hs :: advanceArgPosToSelbri
    /// Advance argument position to selbri (position 2 if no args yet)
    pub fn advance_arg_pos_to_selbri(&mut self) {
        if self.arglist.args.is_empty() {
            self.arglist.position = 2;
        }
    }
    
    // Ported from: ParseM.hs :: setArgInArglist
    /// Set an argument in the arglist
    pub fn set_arg_in_arglist(&mut self, pos: ArgPos, term: JboTerm) {
        self.arglist.args.insert(pos, term);
    }
    
    // Ported from: ParseM.hs :: setArgPos
    /// Set the current argument position
    pub fn set_arg_pos(&mut self, pos: i32) {
        self.arglist.position = pos;
    }
}

// ============================================================================
// Lambda handling
// ============================================================================


pub fn shunt_lambdas(parse_state: &mut ParseState, delta: i32) {
    let mut new_lambdas = HashMap::new();
    for (pos, v) in parse_state.lambdas.iter() {
        if pos.level > 0 {
            let new_pos = LambdaPos {
                level: pos.level + delta,
                num: pos.num,
            };
            new_lambdas.insert(new_pos, *v);
        }
    }
    parse_state.lambdas = new_lambdas;
}

// ============================================================================
// Bridi conversion functions
// ============================================================================

pub fn bridi_to_jbo_vpred(bridi: Bridi) -> JboVPred {
    Arc::new(move |os: &[JboTerm]| {
        let args: Args = os.iter()
            .enumerate()
            .map(|(n, o)| (ArgPos::UnfilledPos(n as i32), o.clone()))
            .collect();
        bridi(&args)
    })
}

pub fn bridi_to_jbo_pred(bridi: Bridi) -> JboPred {
    v_pred_to_pred(bridi_to_jbo_vpred(bridi))
}

// ============================================================================
// Question handling
// ============================================================================

pub fn with_questions<P: PreProp>(top: bool, parse_state: &mut ParseState, p: P) -> P {
    let saved_questions = parse_state.questions.clone();
    parse_state.questions.clear();
    let result = do_questions(top, parse_state, p);
    parse_state.questions.extend(saved_questions);
    result
}

pub fn do_questions<P: PreProp>(top: bool, parse_state: &mut ParseState, p: P) -> P {
    let qinfos = de_kau(top, parse_state);
    qinfos.into_iter().fold(p, |acc, qinfo| do_q_info(qinfo, acc))
}

pub fn do_q_info<P: PreProp>(qinfo: QInfo, p: P) -> P {
    match qinfo {
        QInfo::QSumti(qv, dom) => {
            let qv_clone = qv.clone();
            p.lift_to_prop(Box::new(move |prop: JboProp| {
                let qv_inner = qv_clone.clone();
                let restriction = dom.clone().map(|pred| {
                    Arc::new(move |v: i32| pred(&JboTerm::BoundVar(v))) as Arc<dyn Fn(i32) -> JboProp + Send + Sync>
                });
                Prop::Quantified(
                    JboQuantifier::QuestionQuantifier,
                    restriction,
                    Arc::new(move |v: i32| sub_term(&qv_inner, &JboTerm::BoundVar(v), &prop)),
                )
            }))
        }
        QInfo::QBridi(qv) => {
            let qv_clone = qv.clone();
            p.lift_to_prop(Box::new(move |prop: JboProp| {
                let qv_inner = qv_clone.clone();
                Prop::Quantified(
                    JboQuantifier::RelQuantifier(Box::new(JboQuantifier::QuestionQuantifier)),
                    None,
                    Arc::new(move |v: i32| sub_rel(&qv_inner, &JboRel::BoundRVar(v), &prop)),
                )
            }))
        }
        QInfo::QTruth => {
            p.lift_to_prop(Box::new(|prop: JboProp| {
                Prop::Modal(JboModalOp::QTruthModal, Box::new(prop))
            }))
        }
    }
}

pub fn de_kau(top: bool, parse_state: &mut ParseState) -> Vec<QInfo> {
    let mut out_qs = Vec::new();
    let mut remaining_qs = Vec::new();
    
    for q in parse_state.questions.iter() {
        if top || q.kau_depth == Some(1) {
            out_qs.push(q.info.clone());
        } else {
            let mut new_q = q.clone();
            if let Some(depth) = new_q.kau_depth {
                new_q.kau_depth = Some(depth - 1);
            }
            remaining_qs.push(new_q);
        }
    }
    
    parse_state.questions = remaining_qs;
    out_qs
}

// ============================================================================
// Argument handling
// ============================================================================

pub fn add_arg(arg: Arg, bridi_state: &mut BridiParseState, parse_state: &mut ParseState) {
    match arg {
        Arg::FAITagged(o) => {
            bridi_state.arglist.args.insert(ArgPos::JaiPos, o);
        }
        Arg::Untagged(o) => {
            let n = bridi_state.arglist.position;
            set_arg_in_arglist(ArgPos::NPos(n), o.clone(), bridi_state);
            if !bridi_state.is_sub_bridi {
                parse_state.map_prop(PropTransform::SubstituteTerm(
                    JboTerm::UnboundSumbasti(crate::jbo_syntax::SumtiAtom::MainBridiSumbasti(n)),
                    o,
                ));
            }
            set_arg_pos(n + 1, bridi_state);
        }
        Arg::FATagged(n, o) => {
            let n = n as i32;
            set_arg_in_arglist(ArgPos::NPos(n), o.clone(), bridi_state);
            if !bridi_state.is_sub_bridi {
                parse_state.map_prop(PropTransform::SubstituteTerm(
                    JboTerm::UnboundSumbasti(crate::jbo_syntax::SumtiAtom::MainBridiSumbasti(n)),
                    o,
                ));
            }
            set_arg_pos(n + 1, bridi_state);
        }
    }
}

pub fn set_arg_in_arglist(pos: ArgPos, term: JboTerm, bridi_state: &mut BridiParseState) {
    bridi_state.arglist.args.insert(pos, term);
}

pub fn set_arg_pos(n: i32, bridi_state: &mut BridiParseState) {
    bridi_state.arglist.position = n;
}

// ============================================================================
// Variable binding utilities
// ============================================================================

pub fn shunt_vars<K: Eq + Clone + Hash, V: Clone, F: Fn(usize) -> K>(
    var_fn: &F,
    bindings: &HashMap<K, V>,
) -> HashMap<K, V> {
    let mut result = bindings.clone();

    // Find the first unbound position
    let max_bound = (1..)
        .find(|&n| !bindings.contains_key(&var_fn(n)))
        .unwrap_or(1);

    // Shunt all variables up by 1
    for n in 1..max_bound {
        if let Some(v) = bindings.get(&var_fn(n)) {
            result.insert(var_fn(n + 1), v.clone());
        }
    }

    result
}

pub fn set_shunting<K: Eq + Clone + Hash, V: Clone, F: Fn(usize) -> K>(
    var_fn: &F,
    value: V,
    bindings: &HashMap<K, V>,
) -> HashMap<K, V> {
    let mut result = shunt_vars(var_fn, bindings);
    result.insert(var_fn(1), value);
    result
}

// ============================================================================
// PreProp trait and implementations
// ============================================================================

pub trait PreProp: Clone {
    fn lift_to_prop(self, f: Box<dyn Fn(JboProp) -> JboProp + Send + Sync>) -> Self;
    fn lift_to_prop2(self, other: Self, f: Box<dyn Fn(JboProp, JboProp) -> JboProp + Send + Sync>) -> Self;
    fn dummy_pre_prop() -> Self;
}

impl PreProp for JboProp {
    fn lift_to_prop(self, f: Box<dyn Fn(JboProp) -> JboProp + Send + Sync>) -> Self {
        f(self)
    }

    fn lift_to_prop2(self, other: Self, f: Box<dyn Fn(JboProp, JboProp) -> JboProp + Send + Sync>) -> Self {
        f(self, other)
    }

    fn dummy_pre_prop() -> Self {
        Prop::Eet
    }
}

impl PreProp for JboPred {
    fn lift_to_prop(self, f: Box<dyn Fn(JboProp) -> JboProp + Send + Sync>) -> Self {
        Arc::new(move |o: &JboTerm| f(self(o)))
    }

    fn lift_to_prop2(self, other: Self, f: Box<dyn Fn(JboProp, JboProp) -> JboProp + Send + Sync>) -> Self {
        Arc::new(move |o: &JboTerm| f(self(o), other(o)))
    }

    fn dummy_pre_prop() -> Self {
        Arc::new(|_| Prop::Eet)
    }
}

impl PreProp for JboNPred {
    fn lift_to_prop(self, f: Box<dyn Fn(JboProp) -> JboProp + Send + Sync>) -> Self {
        let pred = self.pred;
        JboNPred {
            arity: self.arity,
            pred: Arc::new(move |os: &[JboTerm]| f(pred(os))),
        }
    }

    fn lift_to_prop2(self, other: Self, f: Box<dyn Fn(JboProp, JboProp) -> JboProp + Send + Sync>) -> Self {
        if self.arity != other.arity {
            panic!("mismatched JboNPred arities in lift_to_prop2");
        }
        let pred1 = self.pred;
        let pred2 = other.pred;
        JboNPred {
            arity: self.arity,
            pred: Arc::new(move |os: &[JboTerm]| f(pred1(os), pred2(os))),
        }
    }

    fn dummy_pre_prop() -> Self {
        JboNPred {
            arity: 0,
            pred: Arc::new(|_| Prop::Eet),
        }
    }
}

// ============================================================================
// Helper functions for substitution
// ============================================================================

fn sub_term(old: &JboTerm, new: &JboTerm, prop: &JboProp) -> JboProp {
    crate::jbo_prop::sub_term(old, new, prop.clone())
}

fn sub_rel(old: &JboRel, new: &JboRel, prop: &JboProp) -> JboProp {
    crate::jbo_prop::sub_rel(old, new, prop.clone())
}

fn jbo_pred_to_loj_pred(pred: &JboPred) -> Arc<dyn Fn(i32) -> JboProp + Send + Sync> {
    crate::jbo_prop::jbo_pred_to_loj_pred(pred)
}

fn v_pred_to_pred(vpred: JboVPred) -> JboPred {
    crate::jbo_prop::v_pred_to_pred(vpred)
}


// ============================================================================
// Arg type definition
// ============================================================================

#[derive(Debug, Clone)]
pub enum Arg {
    Untagged(JboTerm),
    FATagged(usize, JboTerm),
    FAITagged(JboTerm),
}

// ============================================================================
// QInfo type definition
// ============================================================================


pub fn do_lambdas(vpred: JboVPred, parse_state: &mut ParseState) -> JboNPred {
    let ls = parse_state.lambdas.clone();
    let arity = ls.keys()
        .filter_map(|pos| if pos.level == 1 { Some(pos.num) } else { None })
        .max()
        .unwrap_or(0)
        .max(1);

    shunt_lambdas(parse_state, -1);

    JboNPred {
        arity: arity as usize,
        pred: Arc::new(move |os: &[JboTerm]| {
            let mut current: JboVPred = vpred.clone();
            for (idx, o) in os.iter().enumerate() {
                let num = (idx + 1) as i32;
                if let Some(v) = ls.get(&LambdaPos { level: 1, num }) {
                    let previous = current.clone();
                    let old = JboTerm::Var(*v);
                    let new = o.clone();
                    current = Arc::new(move |rest: &[JboTerm]| {
                        crate::jbo_prop::sub_term(&old, &new, previous(rest))
                    });
                } else {
                    let previous = current.clone();
                    let head = o.clone();
                    current = Arc::new(move |rest: &[JboTerm]| {
                        let mut args = vec![head.clone()];
                        args.extend_from_slice(rest);
                        previous(&args)
                    });
                }
            }
            current(&[])
        }),
    }
}

// ============================================================================
// Additional ParseM.hs functions
// ============================================================================

// Ported from: ParseM.hs :: mPredToVDom
/// Convert Maybe JboPred to VariableDomain
pub fn m_pred_to_v_dom(pred: Option<JboPred>) -> VariableDomain {
    match pred {
        Some(p) => VariableDomain::PredDomain(p),
        None => VariableDomain::UnrestrictedDomain,
    }
}

// Ported from: ParseM.hs :: getSumbastiBinding
/// Get sumbasti binding, checking both referenced and unreferenced
pub fn get_sumbasti_binding(bs: &SumbastiBindings, atom: &SumtiAtom) -> JboTerm {
    let key_ref = Sumbasti { explicitly_assigned: true, atom: atom.clone() };
    if let Some(term) = bs.get(&key_ref) {
        return term.clone();
    }
    let key_unref = Sumbasti { explicitly_assigned: false, atom: atom.clone() };
    if let Some(term) = bs.get(&key_unref) {
        return term.clone();
    }
    JboTerm::UnboundSumbasti(atom.clone())
}

// Ported from: ParseM.hs :: getBribastiBinding
/// Get bribasti binding, defaulting to jboRelToBridi if not found
pub fn get_bribasti_binding(bs: &BribastiBindings, tu: &TanruUnit) -> Bridi {
    if let Some(stored) = bs.get(tu) {
        let stored = stored.clone();
        return Box::new(move |args: &Args| stored(args));
    }
    let rel = match tu {
        TanruUnit::TUBrivla(s) => JboRel::Brivla(s.clone()),
        _ => JboRel::UnboundBribasti(tu.clone()),
    };
    jbo_rel_to_bridi(rel)
}

// Ported from: ParseM.hs :: addSideTexticule
/// Add a side texticule to parse state
pub fn add_side_texticule(side: Texticule, parse_state: &mut ParseState) {
    parse_state.side_texticules.insert(0, side);
}

// Ported from: ParseM.hs :: takeSideTexticules
/// Take all side texticules and clear the list
pub fn take_side_texticules(parse_state: &mut ParseState) -> Vec<Texticule> {
    let result = parse_state.side_texticules.clone();
    parse_state.side_texticules.clear();
    result
}

// Ported from: ParseM.hs :: addSumtiQuestion
/// Add a sumti question and return the variable
pub fn add_sumti_question(
    kau: Option<i32>,
    dom: Option<&JboPred>,
    parse_state: &mut ParseState,
) -> JboTerm {
    let v_dom = match dom {
        Some(pred) => VariableDomain::PredDomain(pred.clone()),
        None => VariableDomain::UnrestrictedDomain,
    };
    let var = get_fresh_var(&v_dom, parse_state);
    let q_info = QInfo::QSumti(var.clone(), dom.cloned());
    let question = Question {
        kau_depth: kau,
        info: q_info,
    };
    parse_state.questions.insert(0, question);
    var
}

// Ported from: ParseM.hs :: addBridiQuestion
/// Add a bridi question and return the relation variable
pub fn add_bridi_question(kau: Option<i32>, parse_state: &mut ParseState) -> JboRel {
    let rvar = get_fresh_rvar(parse_state);
    let q_info = QInfo::QBridi(rvar.clone());
    let question = Question {
        kau_depth: kau,
        info: q_info,
    };
    parse_state.questions.push(question);
    rvar
}

// Helper to get fresh var
fn get_fresh_var(dom: &VariableDomain, parse_state: &mut ParseState) -> JboTerm {
    let v = parse_state.next_fresh_var;
    parse_state.next_fresh_var += 1;
    parse_state.variable_domains.insert(v, dom.clone());
    JboTerm::Var(v)
}

// Helper to get fresh rvar
fn get_fresh_rvar(parse_state: &mut ParseState) -> JboRel {
    let v = parse_state.next_fresh_rvar;
    parse_state.next_fresh_rvar += 1;
    JboRel::RVar(v)
}

// ============================================================================
// Additional ParseM.hs functions (continued)
// ============================================================================

// Ported from: ParseM.hs :: advanceArgPosToSelbri
/// Advance argument position to selbri (position 2) if no args yet
pub fn advance_arg_pos_to_selbri(bridi_state: &mut BridiParseState) {
    if bridi_state.arglist.args.is_empty() {
        bridi_state.arglist.position = 2;
    }
}

// Ported from: ParseM.hs :: ignoringArgs
/// Execute action while preserving arglist state
pub fn ignoring_args<F, R>(bridi_state: &mut BridiParseState, f: F) -> R
where
    F: FnOnce(&mut BridiParseState) -> R,
{
    let saved_arglist = bridi_state.arglist.clone();
    let result = f(bridi_state);
    bridi_state.arglist = saved_arglist;
    result
}

// Ported from: ParseM.hs :: updateReferenced
/// Mark a variable as referenced
pub fn update_referenced(term: &JboTerm, parse_state: &mut ParseState) {
    if let JboTerm::Var(n) = term {
        parse_state.referenced_vars.insert(*n);
    }
}

// Ported from: ParseM.hs :: doAssigns
/// Process a list of assignments
pub fn do_assigns(
    term: JboTerm,
    assigns: &[Either<SumtiAtom, JboTerm>],
    parse_state: &mut ParseState,
) -> JboTerm {
    let mut result = term;
    for assign in assigns {
        result = do_assign(result, assign, parse_state);
    }
    result
}

// Ported from: ParseM.hs :: doAssign
/// Process a single assignment
pub fn do_assign(
    term: JboTerm,
    assign: &Either<SumtiAtom, JboTerm>,
    parse_state: &mut ParseState,
) -> JboTerm {
    match (&term, assign) {
        (JboTerm::UnboundSumbasti(a), Either::Right(assto)) if is_assignable(a) => {
            let key = Sumbasti {
                explicitly_assigned: true,
                atom: a.clone(),
            };
            parse_state.sumbasti_bindings.insert(key, assto.clone());
            assto.clone()
        }
        (_, Either::Left(ass)) => {
            let key = Sumbasti {
                explicitly_assigned: true,
                atom: ass.clone(),
            };
            parse_state.sumbasti_bindings.insert(key, term.clone());
            term
        }
        _ => term,
    }
}

// Helper for doAssign
// Ported from: JboSyntax.hs :: isAssignable
fn is_assignable(atom: &SumtiAtom) -> bool {
    matches!(
        atom,
        SumtiAtom::Assignable(_) | SumtiAtom::LerfuString(_) | SumtiAtom::Name(_, _, _)
    )
}

// Helper type for Either
#[derive(Debug, Clone)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

// Ported from: ParseM.hs :: doIncidentals
/// Process incidental clauses - adds side texticules for non-veridical propositions
pub fn do_incidentals(
    term: JboTerm,
    incidentals: &[JboPred],
    parse_state: &mut ParseState,
) -> JboTerm {
    let Some(pred) = crate::jbo_prop::and_m_pred(incidentals.to_vec()) else {
        return term;
    };

    let (pred, term) = quantify_out_frees(true, pred, term, parse_state);
    let prop = pred(&term);
    extract_non_veridical(&prop, &term, parse_state);
    parse_state.add_side_texticule(Texticule::TexticuleProp(prop));
    term
}

fn quantify_out_frees(
    add_params: bool,
    pred: JboPred,
    term: JboTerm,
    parse_state: &mut ParseState,
) -> (JboPred, JboTerm) {
    let frees = crate::jbo_prop::free_vars(&pred(&term));
    if frees.is_empty() {
        return (pred, term);
    }

    let term = if add_params {
        match term {
            JboTerm::Constant(n, params) => {
                let mut new_params = frees.iter().rev().cloned().collect::<Vec<_>>();
                new_params.extend(params);
                JboTerm::Constant(n, new_params)
            }
            other => other,
        }
    } else {
        term
    };

    let mut pred = pred;
    for free in frees {
        let domain = match free {
            JboTerm::Var(n) => parse_state.variable_domains.get(&n).cloned().unwrap_or(VariableDomain::UnrestrictedDomain),
            _ => VariableDomain::UnrestrictedDomain,
        };
        pred = quantify_out_free(pred, free, domain);
    }

    quantify_out_frees(false, pred, term, parse_state)
}

fn quantify_out_free(pred: JboPred, free: JboTerm, domain: VariableDomain) -> JboPred {
    Arc::new(move |x: &JboTerm| {
        let body = pred(x);
        let free_for_body = free.clone();
        let restriction = restr_of_domain(domain.clone());
        Prop::Quantified(
            JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Forall),
            restriction.map(|r| {
                Arc::new(move |v: i32| r(&JboTerm::BoundVar(v))) as Arc<dyn Fn(i32) -> JboProp + Send + Sync>
            }),
            Arc::new(move |v: i32| crate::jbo_prop::sub_term(&free_for_body, &JboTerm::BoundVar(v), body.clone())),
        )
    })
}

fn restr_of_domain(domain: VariableDomain) -> Option<JboPred> {
    match domain {
        VariableDomain::UnrestrictedDomain => None,
        VariableDomain::PredDomain(pred) => Some(pred),
        VariableDomain::FiniteDomain(os) => {
            let first = os.first()?.clone();
            let rest = os[1..].to_vec();
            Some(Arc::new(move |x: &JboTerm| {
                rest.iter().rev().fold(
                    Prop::Rel(JboRel::Equal, vec![x.clone(), first.clone()]),
                    |acc, o| Prop::Connected(
                        crate::logic::Connective::Or,
                        Box::new(Prop::Rel(JboRel::Equal, vec![x.clone(), o.clone()])),
                        Box::new(acc),
                    ),
                )
            }))
        }
    }
}

/// Extract non-veridical propositions from a proposition and add to parse state
fn extract_non_veridical(prop: &JboProp, _term: &JboTerm, parse_state: &mut ParseState) {
    if let Prop::Modal(JboModalOp::NonVeridical, inner) = prop {
        parse_state.non_veridical_props.push(inner.as_ref().clone());
    }
}

pub fn apply_transform_list(transforms: Vec<PropTransform>, mut prop: JboProp) -> JboProp {
    for transform in transforms.into_iter().rev() {
        prop = apply_transform(transform, prop);
    }
    prop
}

fn continuation_args_from_prop(prop: &JboProp, start_position: i32) -> Args {
    let Prop::Rel(_, terms) = prop else {
        return Args::new();
    };
    terms
        .iter()
        .enumerate()
        .filter_map(|(idx, term)| {
            let position = idx as i32 + 1;
            if position >= start_position {
                Some((ArgPos::UnfilledPos(position - start_position), term.clone()))
            } else {
                None
            }
        })
        .collect()
}

fn fill_args_in_prop(prop: JboProp, args: &Args) -> JboProp {
    let terms = args_to_jboterms(args);
    match prop {
        Prop::Rel(rel, _) => Prop::Rel(rel, terms),
        Prop::Not(p) => Prop::Not(Box::new(fill_args_in_prop(*p, args))),
        Prop::Connected(c, p1, p2) => Prop::Connected(
            c,
            Box::new(fill_args_in_prop(*p1, args)),
            Box::new(fill_args_in_prop(*p2, args)),
        ),
        Prop::NonLogConnected(j, p1, p2) => Prop::NonLogConnected(
            j,
            Box::new(fill_args_in_prop(*p1, args)),
            Box::new(fill_args_in_prop(*p2, args)),
        ),
        Prop::Modal(m, p) => Prop::Modal(m, Box::new(fill_args_in_prop(*p, args))),
        Prop::Quantified(q, pred, body) => {
            let args = args.clone();
            Prop::Quantified(q, pred, Arc::new(move |v| fill_args_in_prop(body(v), &args)))
        }
        Prop::Eet => Prop::Eet,
    }
}

// Apply a single transformation to a proposition
fn apply_transform(transform: PropTransform, prop: JboProp) -> JboProp {
    match transform {
        PropTransform::Quantify(fresh_var, quantifier, restriction) => {
            if let JboTerm::Var(n) = fresh_var {
                let pred = restriction.map(|f| {
                    Arc::new(move |v: i32| f(v)) as Arc<dyn Fn(i32) -> JboProp + Send + Sync>
                });
                Prop::Quantified(
                    quantifier,
                    pred,
                    Arc::new(move |v: i32| crate::jbo_prop::sub_term(&JboTerm::Var(n), &JboTerm::BoundVar(v), prop.clone()))
                )
            } else {
                prop
            }
        }
        PropTransform::ConnectQuantify { conn, fresh, left_quant, right_quant, restriction } => {
            let left_fresh = fresh.clone();
            let left_prop = prop.clone();
            let left_restriction = restriction.clone();
            let left = Prop::Quantified(
                left_quant,
                left_restriction,
                Arc::new(move |v: i32| crate::jbo_prop::sub_term(&left_fresh, &JboTerm::BoundVar(v), left_prop.clone())),
            );
            let right = Prop::Quantified(
                right_quant,
                restriction,
                Arc::new(move |v: i32| crate::jbo_prop::sub_term(&fresh, &JboTerm::BoundVar(v), prop.clone())),
            );
            crate::jbo_prop::conn_to_fol(&conn, left, right)
        }
        PropTransform::ConnectQuantifyTagged { conn, fresh, left_quant, right_quant, restriction, event_fresh, left_modal, right_modal } => {
            let left_fresh = fresh.clone();
            let left_prop = prop.clone();
            let left_restriction = restriction.clone();
            let left = Prop::Quantified(
                left_quant,
                left_restriction,
                Arc::new(move |v: i32| crate::jbo_prop::sub_term(&left_fresh, &JboTerm::BoundVar(v), left_prop.clone())),
            );
            let left = if let Some(modal) = left_modal {
                Prop::Modal(modal, Box::new(left))
            } else {
                left
            };
            let right = Prop::Quantified(
                right_quant,
                restriction,
                Arc::new(move |v: i32| crate::jbo_prop::sub_term(&fresh, &JboTerm::BoundVar(v), prop.clone())),
            );
            let right = if let Some(modal) = right_modal {
                Prop::Modal(modal, Box::new(right))
            } else {
                right
            };
            let connected = crate::jbo_prop::conn_to_fol(&conn, left, right);
            if let Some(JboTerm::Var(n)) = event_fresh {
                Prop::Quantified(
                    JboQuantifier::LojQuantifier(crate::logic::LojQuantifier::Exists),
                    None,
                    Arc::new(move |v: i32| crate::jbo_prop::sub_term(&JboTerm::Var(n), &JboTerm::BoundVar(v), connected.clone()))
                )
            } else {
                connected
            }
        }
        PropTransform::RQuantify(fresh_rel, quantifier) => {
            Prop::Quantified(
                quantifier,
                None,
                Arc::new(move |v: i32| crate::jbo_prop::sub_rel(&fresh_rel, &JboRel::BoundRVar(v), prop.clone()))
            )
        }
        PropTransform::SubstituteTerm(old_term, new_term) => crate::jbo_prop::sub_term(&old_term, &new_term, prop),
        PropTransform::LogicalConnectBranches { conn, fresh, left_term, left_transforms, right_term, right_transforms } => {
            let left_base = crate::jbo_prop::sub_term(&fresh, &left_term, prop.clone());
            let right_base = crate::jbo_prop::sub_term(&fresh, &right_term, prop);
            let left = apply_transform_list(left_transforms, left_base);
            let right = apply_transform_list(right_transforms, right_base);
            crate::jbo_prop::conn_to_fol(&conn, left, right)
        }
        PropTransform::ConnectBranches { conn, left_args, right_args, continuation_position, left_transforms, right_transforms, outer_transforms } => {
            let continuation_args = continuation_args_from_prop(&prop, continuation_position);
            let left_args = join_args(&continuation_args, &left_args);
            let right_args = join_args(&continuation_args, &right_args);
            let left = apply_transform_list(left_transforms, fill_args_in_prop(prop.clone(), &left_args));
            let right = apply_transform_list(right_transforms, fill_args_in_prop(prop, &right_args));
            let connected = match conn {
                crate::jbo_syntax::Connective::JboConnLog(_, lcon) => crate::jbo_prop::conn_to_fol(&lcon, left, right),
                crate::jbo_syntax::Connective::JboConnJoik(_, joik) => crate::jbo_prop::joik_to_fol(joik, left, right),
            };
            apply_transform_list(outer_transforms, connected)
        }
        PropTransform::ConnectTagBranches { conn, left_transforms, right_transforms } => {
            let left = apply_transform_list(left_transforms, prop.clone());
            let right = apply_transform_list(right_transforms, prop);
            match conn {
                crate::jbo_prop::JboConnective::JboConnLog(_, lcon) => crate::jbo_prop::conn_to_fol(&lcon, left, right),
                crate::jbo_prop::JboConnective::JboConnJoik(_, joik) => crate::jbo_prop::joik_to_fol(joik, left, right),
            }
        }
        PropTransform::Negate => Prop::Not(Box::new(prop)),
        PropTransform::ApplyModal(modal_op) => Prop::Modal(modal_op, Box::new(prop)),
    }
}

impl std::fmt::Debug for PropTransform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PropTransform::Quantify(term, quant, _) => write!(f, "Quantify({:?}, {:?}, <fn>)", term, quant),
            PropTransform::ConnectQuantify { conn, fresh, .. } => write!(f, "ConnectQuantify({:?}, {:?})", conn, fresh),
            PropTransform::ConnectQuantifyTagged { conn, fresh, .. } => write!(f, "ConnectQuantifyTagged({:?}, {:?})", conn, fresh),
            PropTransform::RQuantify(rel, quant) => write!(f, "RQuantify({:?}, {:?})", rel, quant),
            PropTransform::SubstituteTerm(old_term, new_term) => write!(f, "SubstituteTerm({:?}, {:?})", old_term, new_term),
            PropTransform::LogicalConnectBranches { conn, fresh, left_term, right_term, .. } => write!(f, "LogicalConnectBranches({:?}, {:?}, {:?}, {:?})", conn, fresh, left_term, right_term),
            PropTransform::ConnectBranches { conn, .. } => write!(f, "ConnectBranches({:?})", conn),
            PropTransform::ConnectTagBranches { conn, .. } => write!(f, "ConnectTagBranches({:?})", conn),
            PropTransform::Negate => write!(f, "Negate"),
            PropTransform::ApplyModal(op) => write!(f, "ApplyModal({:?})", op),
        }
    }
}
