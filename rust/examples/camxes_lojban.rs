//! Load the Lojban PEG grammar from `lojban.peg` and parse input from stdin or argv.
//!
//! This is a dev-UX shim used for diagnosing `Peg::new` failures and smoke-testing
//! the grammar against short utterances (see `plan.md`, Phase 1).

use tersmu::camxes::peg::grammar::Peg;
use tersmu::camxes::LOJBAN_GRAMMAR;
use std::io::Read;


fn main() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    let peg = match Peg::new(LOJBAN_GRAMMAR.0, LOJBAN_GRAMMAR.1) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Peg::new failed: {e:?}");
            std::process::exit(2);
        }
    };
    eprintln!("grammar loaded; start rule = 'text'");

    let input: String = match std::env::args().nth(1) {
        Some(arg) => arg,
        None => {
            let mut s = String::new();
            std::io::stdin().read_to_string(&mut s).expect("stdin");
            s
        }
    };

    let result = peg.parse(input.trim());
    match result.3.as_ref() {
        Ok(nodes) => {
            println!("parsed {} root node(s), final_pos = {}, error_pos = {}", nodes.len(), result.1, result.2);
        }
        Err(e) => {
            eprintln!("parse error: {e}");
            std::process::exit(1);
        }
    }
}
