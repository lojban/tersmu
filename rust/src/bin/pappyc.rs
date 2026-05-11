//! Rust **pappyc**: compile `.pappy` grammars to Rust (port of `pappy/pappy/Main.hs` + `WriteParser.hs`).

use std::env;
use std::path::PathBuf;

fn usage() -> ! {
    eprintln!("Usage: pappyc [-h] [-v] [-o OUT.rs] INPUT.pappy (see pappyc --help)");
    std::process::exit(2);
}

fn print_help() -> ! {
    eprintln!("Usage: pappyc [-h] [-v] [-o OUT.rs] INPUT.pappy");
    eprintln!("Compile a .pappy grammar to Rust (tersmu packrat backend).");
    eprintln!();
    eprintln!("  -h, --help      Print help and exit");
    eprintln!("  -v, --verbose   Log input and output paths");
    eprintln!("  -o, --output    Write Rust to this path (default: INPUT with .rs extension)");
    std::process::exit(0);
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut verbose = false;
    let mut out: Option<PathBuf> = None;
    let mut input: Option<PathBuf> = None;
    let mut it = args.into_iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "-h" | "--help" => print_help(),
            "-v" | "--verbose" => verbose = true,
            "-o" | "--output" => {
                out = Some(PathBuf::from(it.next().unwrap_or_else(|| usage())));
            }
            _ if a.starts_with('-') => usage(),
            _ => {
                if input.is_some() {
                    usage();
                }
                input = Some(PathBuf::from(a));
            }
        }
    }
    let input = input.unwrap_or_else(|| usage());
    let out = out.unwrap_or_else(|| {
        let mut p = input.clone();
        p.set_extension("rs");
        p
    });

    let src = std::fs::read_to_string(&input).unwrap_or_else(|e| {
        eprintln!("pappyc: {}: {e}", input.display());
        std::process::exit(1);
    });
    let name = input.file_name().and_then(|s| s.to_str()).unwrap_or("grammar.pappy");

    if verbose {
        eprintln!("pappyc: input={} output={}", input.display(), out.display());
    }

    let rust = tersmu::pappyc::compile_to_rust(name, &src).unwrap_or_else(|e| {
        eprintln!("pappyc: {e}");
        std::process::exit(1);
    });

    std::fs::write(&out, rust).unwrap_or_else(|e| {
        eprintln!("pappyc: {}: {e}", out.display());
        std::process::exit(1);
    });
    if verbose {
        eprintln!("pappyc: wrote {}", out.display());
    }
}
