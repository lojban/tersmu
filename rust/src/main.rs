//! Native Rust `tersmu` CLI ([Main.hs](../Main.hs)). Grammars: Pappy (`.pappy` / `.pappy.rhs`), not Pest.

#![allow(dead_code)]
#![allow(unused_imports)]

fn main() {
    env_logger::init();
    let argv: Vec<String> = std::env::args().skip(1).collect();
    let (opts, rest) = match tersmu::cli::parse_args(&argv) {
        Ok(x) => x,
        Err(e) => {
            log::error!("{e}");
            std::process::exit(2);
        }
    };
    if let Err(e) = tersmu::run::main_with_args(opts, rest) {
        log::error!("{e}");
        std::process::exit(1);
    }
}
