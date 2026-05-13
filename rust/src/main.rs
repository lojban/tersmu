//! Native Rust `camxes` CLI - Lojban parser with semantic analysis.

#![allow(dead_code)]
#![allow(unused_imports)]

fn main() {
    env_logger::init();
    let argv: Vec<String> = std::env::args().skip(1).collect();
    let (opts, rest) = match camxes_rs::cli::parse_args(&argv) {
        Ok(x) => x,
        Err(e) => {
            log::error!("{e}");
            std::process::exit(2);
        }
    };
    if let Err(e) = camxes_rs::run::main_with_args(opts, rest) {
        log::error!("{e}");
        std::process::exit(1);
    }
}
