//! Helper functions for ParseM
//!
//! Ported from: ParseM.hs (helper functions section)

use crate::jbo_prop::{JboRel, JboTag, JboTerm};
use crate::jbo_syntax::{SumtiAtom, TanruUnit};
use crate::parse_m::{Bridi, Args, SumbastiBindings, BribastiBindings, Sumbasti, jbo_rel_to_bridi, args_to_jboterms, ArgPos};
use std::collections::HashMap;

// Ported from: ParseM.hs :: getSumbastiBinding
/// Get sumbasti binding by SumtiAtom key
pub fn get_sumbasti_binding(bindings: &SumbastiBindings, atom: &SumtiAtom) -> JboTerm {
    // Try explicitly assigned first
    let explicit_key = Sumbasti {
        explicitly_assigned: true,
        atom: atom.clone(),
    };
    if let Some(term) = bindings.get(&explicit_key) {
        return term.clone();
    }

    // Try non-explicit
    let implicit_key = Sumbasti {
        explicitly_assigned: false,
        atom: atom.clone(),
    };
    if let Some(term) = bindings.get(&implicit_key) {
        return term.clone();
    }

    // Default: unbound
    JboTerm::UnboundSumbasti(atom.clone())
}

// Ported from: ParseM.hs :: getBribastiBinding
/// Get bribasti binding or convert TanruUnit to default bridi
pub fn get_bribasti_binding(bindings: &BribastiBindings, tu: &TanruUnit) -> Bridi {
    if let Some(stored) = bindings.get(tu) {
        let stored = stored.clone();
        Box::new(move |args: &Args| stored(args))
    } else {
        // Default: convert TanruUnit to brivla relation
        match tu {
            TanruUnit::TUBrivla(name) => {
                let rel = JboRel::Brivla(name.clone());
                jbo_rel_to_bridi(rel)
            }
            _ => {
                // ParseM.hs expects an existing binding for non-brivla bribasti; use `broda` only for integration recovery.
                let rel = JboRel::Brivla("broda".to_string());
                jbo_rel_to_bridi(rel)
            }
        }
    }
}

// Ported from: ParseM.hs :: withJaiAsTag
/// Wrap a bridi with JAI as a modal tag
pub fn with_jai_as_tag(jtag: JboTag, bridi: Bridi) -> Bridi {
    Box::new(move |args: &Args| {
        let jai_term = args.get(&ArgPos::JaiPos).cloned();
        let mut new_args = args.clone();
        new_args.remove(&ArgPos::JaiPos);

        let inner_prop = bridi(&new_args);
        crate::logic::Prop::Modal(
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

            // Wrap with tu'a qualifier
            let qualified = JboTerm::NonAnaph(format!("tu'a({:?})", jai_term));
            new_args.insert(ArgPos::NPos(1), qualified);

            bridi(&new_args)
        } else {
            bridi(args)
        }
    })
}

// Ported from: ParseM.hs :: swapTerms
/// Swap two terms in a list by position (1-indexed)
pub fn swap_terms(terms: Vec<JboTerm>, n: i32, m: i32) -> Vec<JboTerm> {
    let mut result = terms;
    let n_idx = (n - 1) as usize;
    let m_idx = (m - 1) as usize;

    if n_idx < result.len() && m_idx < result.len() {
        result.swap(n_idx, m_idx);
    }

    result
}
