//! Native Rust `tersmu` CLI ([Main.hs](../Main.hs)). Grammars: Pappy (`.pappy` / `.pappy.rhs`), not Pest.

#![allow(dead_code)]
#![allow(unused_imports)]

fn main() {
    let argv: Vec<String> = std::env::args().skip(1).collect();
    let (opts, rest) = match tersmu::cli::parse_args(&argv) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(2);
        }
    };
    if let Err(e) = tersmu::run::main_with_args(opts, rest) {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
