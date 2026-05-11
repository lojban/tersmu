//! Proper morphology validation using camxes-rs word-level parsing.
//! 
//! Integration glue for validating words against the camxes-rs `lojban_word` rule while matching
//! the Haskell Morphology.pappy acceptance behavior.

use crate::camxes::peg::grammar::Peg;
use std::cell::OnceCell;

thread_local! {
    static WORD_PEG: OnceCell<Peg> = const { OnceCell::new() };
}

fn with_word_peg<R>(f: impl FnOnce(&Peg) -> Result<R, usize>) -> Result<R, usize> {
    WORD_PEG.with(|cell| {
        if cell.get().is_none() {
            let (_, grammar) = crate::camxes::LOJBAN_GRAMMAR;
            let peg = Peg::new("lojban_word", grammar).map_err(|_| 0usize)?;
            let _ = cell.set(peg);
        }
        let peg = cell.get().ok_or(0usize)?;
        f(peg)
    })
}

/// Validate a single word using camxes-rs lojban_word rule
fn validate_word(word: &str) -> bool {
    with_word_peg(|peg| {
        match peg.parse(word) {
            Ok(_) => Ok(true),
            Err(_) => Ok(false),
        }
    }).unwrap_or(false)
}

/// Validate morphology of space-separated words
pub fn validate_morphology(words: &str) -> Result<(), usize> {
    let mut pos = 0;
    for word in words.split_whitespace() {
        if !validate_word(word) {
            return Err(pos);
        }
        pos += word.len() + 1; // +1 for space
    }
    Ok(())
}
