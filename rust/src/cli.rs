//! CLI options matching [Main.hs](../Main.hs) (`System.Console.GetOpt`).

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputType {
    #[default]
    Both,
    Jbo,
    Loj,
    Prolog,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum InputType {
    #[default]
    WholeText,
    Lines,
    Paras,
}

#[derive(Debug, Clone, Default)]
pub struct Options {
    pub output: OutputType,
    pub input: InputType,
    pub utf8: bool,
    pub json: bool,
}

pub fn print_version() {
    println!("1.0.0");
}

pub fn print_help() {
    eprintln!(
        "Usage: camxes [OPTION...] [in] [out]\n\
         \t(use '-' for stdin/stdout)\n\
         Options:\n\
         \t-l  --loj      output logical form only\n\
         \t-j  --jbo      output forethoughtful lojbanic form only\n\
         \t-P  --prolog   output Prolog (SWI-Prolog) source\n\
         \t-L  --lines    interpret each line as a lojban text\n\
         \t-p  --paragraphs  interpret each blank-line-separated paragraph as a lojban text\n\
         \t-u  --utf8     output utf8 encoded text rather than ascii\n\
         \t-v  --version  show version\n\
         \t-h  --help     show help\n\
         \t    --json     output one JSON object per line (NDJSON)\n"
    );
}

/// Parse argv like Main.hs: last repeated flag wins for output/input mode.
pub fn parse_args(args: &[String]) -> Result<(Options, Vec<String>), String> {
    let mut opts = Options::default();
    let mut rest = Vec::new();
    let mut i = 0;
    while i < args.len() {
        let a = &args[i];
        match a.as_str() {
            "-h" | "--help" => {
                print_help();
                std::process::exit(0);
            }
            "-v" | "--version" => {
                print_version();
                std::process::exit(0);
            }
            "-l" | "--loj" => opts.output = OutputType::Loj,
            "-j" | "--jbo" => opts.output = OutputType::Jbo,
            "-P" | "--prolog" => opts.output = OutputType::Prolog,
            "-L" | "--lines" => opts.input = InputType::Lines,
            "-p" | "--paragraphs" => opts.input = InputType::Paras,
            "-u" | "--utf8" => opts.utf8 = true,
            "--json" => opts.json = true,
            // Match Haskell GetOpt: bare `-` is stdin/stdout, not a flag (see `examples/tersmuLines`).
            "-" => rest.push(a.clone()),
            s if s.starts_with('-') => {
                return Err(format!("unknown option {s}\nRun with --help for usage."));
            }
            _ => rest.push(a.clone()),
        }
        i += 1;
    }
    Ok((opts, rest))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bare_dash_is_stdin_not_unknown_option() {
        let (_opts, rest) = parse_args(&["-".to_string()]).expect("parse");
        assert_eq!(rest, vec!["-".to_string()]);
    }
}
