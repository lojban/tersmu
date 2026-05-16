//! Main loop: [Main.hs](../Main.hs) `doParse`, `mangleInput`, `parseLineToResult`, `jsonOneLine`.

use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;

use crate::cli::{InputType, Options, OutputType};
use crate::eval_show::{eval_text_to_outputs_with_options, eval_text_to_prolog};
use crate::morphology;

/// ASCII fallback for logical output when `--utf8` is not set ([JboShow.hs](../JboShow.hs) `asciifyJboShown`).
pub fn asciify_jbo_shown(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '≥' => out.push_str(">="),
            '≤' => out.push_str("=<"),
            '→' => out.push_str("-->"),
            '↔' => out.push_str("<->"),
            '¬' => out.push('!'),
            '∀' => out.push_str("FA "),
            '∃' => out.push_str("EX "),
            '∧' => out.push_str("/\\"),
            '∨' => out.push_str("\\/"),
            '«' => out.push_str("<< "),
            '»' => out.push_str(" >>"),
            _ => out.push(c),
        }
    }
    out
}

pub fn trim_str(s: &str) -> String {
    s.trim_matches(|c: char| matches!(c, ' ' | '\t' | '\n' | '\r'))
        .to_string()
}

fn highlight_error(h: &mut dyn Write, pos: usize, s: &str, errstr: &str) -> io::Result<()> {
    let context = 40usize;
    let start = pos.saturating_sub(context);
    let slice: String = s.chars().skip(start).take(context * 2).collect();
    // `pos == 0`: align `^` with the first column inside `{…}` (examples/3.loj morphology).
    let indent = pos.saturating_sub(start);
    let spaces = " ".repeat(indent);
    let body = if errstr == "Morphology error" {
        format!("{} ", slice.trim_end())
    } else {
        slice
    };
    writeln!(
        h,
        "{}:\n\t{{{}}}\n\t {}^\n",
        errstr, body, spaces
    )
}

fn mangle_input(mode: InputType, s: &str) -> Vec<String> {
    match mode {
        InputType::WholeText => vec![s
            .chars()
            .map(|c| if matches!(c, '\n' | '\r') { ' ' } else { c })
            .collect()],
        InputType::Lines => s.lines().map(String::from).collect(),
        InputType::Paras => split_at_nulls(s.lines().collect()),
    }
}

fn split_at_nulls(lines: Vec<&str>) -> Vec<String> {
    let ls: Vec<String> = lines.into_iter().map(String::from).collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < ls.len() {
        let mut chunk = Vec::new();
        while i < ls.len() && !ls[i].is_empty() {
            chunk.push(ls[i].clone());
            i += 1;
        }
        if !chunk.is_empty() {
            out.push(chunk.join(" "));
        }
        while i < ls.len() && ls[i].is_empty() {
            i += 1;
        }
    }
    out
}

fn morph_append_end(text: &str) -> String {
    format!("{text} %%%END%%%")
}

pub fn parse_line_to_result(line: &str) -> Result<(String, String, String, String), String> {
    let text = match morphology::morph(line) {
        Ok(t) => t,
        Err(p) => return Err(error_message("Morphology error", p, line)),
    };
    let with_end = morph_append_end(&text);
    match crate::parse_lojban::parse_text(&with_end) {
        Ok(parsed) => {
            let (logical, canonical, graph) = eval_text_to_outputs_with_options(&parsed, true);
            let prolog = eval_text_to_prolog(&parsed);
            Ok((logical, canonical, graph, prolog))
        }
        Err(p) => Err(error_message("Parse error", p, &text)),
    }
}

fn error_message(errstr: &str, pos: usize, s: &str) -> String {
    let context = 40usize;
    let start = pos.saturating_sub(context);
    let slice: String = s.chars().skip(start).take(context * 2).collect();
    let indent = pos.saturating_sub(start);
    let spaces = " ".repeat(indent);
    let body = if errstr == "Morphology error" {
        format!("{} ", slice.trim_end())
    } else {
        slice
    };
    format!(
        "{}:\n\t{{{}}}\n\t {}^\n",
        errstr, body, spaces
    )
}

fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(c),
        }
    }
    out
}

pub fn json_one_line(opts: &Options, input: &str, result: &Result<(String, String, String, String), String>) -> String {
    let enc = if opts.utf8 {
        |s: String| s
    } else {
        |s: String| asciify_jbo_shown(&s)
    };
    match result {
        Err(e) => format!(
            "{{\"input\":\"{}\",\"logical\":null,\"canonical\":null,\"graph\":null,\"prolog\":null,\"error\":\"{}\"}}",
            json_escape(&trim_str(input)),
            json_escape(&trim_str(&enc(e.clone())))
        ),
        Ok((loj, jbo, graph, prolog)) => format!(
            "{{\"input\":\"{}\",\"logical\":\"{}\",\"canonical\":\"{}\",\"graph\":{},\"prolog\":\"{}\",\"error\":null}}",
            json_escape(&trim_str(input)),
            json_escape(&trim_str(&enc(loj.clone()))),
            json_escape(&trim_str(&enc(jbo.clone()))),
            graph,
            json_escape(prolog)
        ),
    }
}

pub fn do_parse(opts: &Options, h: &mut dyn Write, herr: &mut dyn Write, s: &str) -> io::Result<()> {
    let text = match morphology::morph(s) {
        Ok(t) => t,
        Err(pos) => {
            highlight_error(herr, pos, s, "Morphology error")?;
            return Ok(());
        }
    };
    let with_end = morph_append_end(&text);
    match crate::parse_lojban::parse_text(&with_end) {
        Ok(parsed) => {
            if opts.output == OutputType::Prolog {
                let prolog = eval_text_to_prolog(&parsed);
                writeln!(h, "{}", prolog)?;
            } else {
                let (logical, canonical, _graph) = eval_text_to_outputs_with_options(&parsed, opts.utf8);
                let (logical, canonical) = if opts.utf8 {
                    (logical, canonical)
                } else {
                    (asciify_jbo_shown(&logical), asciify_jbo_shown(&canonical))
                };
                if opts.json {
                    let _ = canonical;
                    let _ = logical;
                } else if !logical.is_empty() || !canonical.is_empty() {
                    match opts.output {
                        OutputType::Both => {
                            write!(h, "{logical}\n\n")?;
                            write!(h, "{canonical}\n\n")?;
                        }
                        OutputType::Loj => {
                            write!(h, "{logical}\n\n")?;
                        }
                        OutputType::Jbo => {
                            write!(h, "{canonical}\n\n")?;
                        }
                        OutputType::Prolog => unreachable!(),
                    }
                }
            }
        }
        Err(pos) => {
            // `parse_text` error positions are in morph-normalized `text`, not `with_end`.
            highlight_error(herr, pos, &text, "Parse error")?;
        }
    }
    Ok(())
}

pub fn repl(opts: Options) -> io::Result<()> {
    let mut stdin = io::stdin();
    let mut stderr = io::stderr();
    let mut stdout = io::stdout();

    // In line mode, read all input at once and process line-by-line
    if opts.input == InputType::Lines {
        let mut input = String::new();
        stdin.read_to_string(&mut input)?;
        for line in input.lines() {
            writeln!(stdout, "> {}", line.trim())?;
            writeln!(stdout)?;
            let line = line.trim();
            if !line.is_empty() {
                do_parse(&opts, &mut stdout, &mut stderr, line)?;
            }
            writeln!(stdout, "-----")?;
        }
        return Ok(());
    }

    // Interactive REPL mode
    loop {
        write!(stderr, "> ")?;
        stderr.flush()?;
        let mut line = String::new();
        if stdin.read_line(&mut line)? == 0 {
            break;
        }
        if opts.json {
            let r = parse_line_to_result(line.trim_end());
            writeln!(stdout, "{}", json_one_line(&opts, line.trim_end(), &r))?;
        } else {
            do_parse(&opts, &mut stdout, &mut stderr, line.trim_end())?;
        }
        writeln!(stderr)?;
    }
    Ok(())
}

pub fn main_with_args(opts: Options, args: Vec<String>) -> io::Result<()> {
    let (input_src, mut out_handle): (Option<String>, Box<dyn Write>) = match args.len() {
        0 => return repl(opts),
        1 => {
            let s = read_input(&args[0])?;
            (Some(s), Box::new(io::stdout()))
        }
        2 => {
            let s = read_input(&args[0])?;
            let h: Box<dyn Write> = if args[1] == "-" {
                Box::new(io::stdout())
            } else {
                Box::new(File::create(Path::new(&args[1]))?)
            };
            (Some(s), h)
        }
        _ => {
            log::error!("too many arguments");
            std::process::exit(2);
        }
    };

    let Some(s) = input_src else {
        unreachable!()
    };

    if opts.json {
        for line in mangle_input(opts.input, &s) {
            let r = parse_line_to_result(&line);
            writeln!(out_handle, "{}", json_one_line(&opts, &line, &r))?;
        }
    } else {
        for chunk in mangle_input(opts.input, &s) {
            // In line mode, echo the input with "> " prefix and add separator
            if opts.input == InputType::Lines {
                writeln!(out_handle, "> {}", chunk.trim())?;
                writeln!(out_handle)?;
            }
            do_parse(&opts, &mut out_handle, &mut io::stderr(), &chunk)?;
            if opts.input == InputType::Lines {
                writeln!(out_handle, "-----")?;
            }
        }
    }
    Ok(())
}

fn read_input(path: &str) -> io::Result<String> {
    if path == "-" {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf)?;
        Ok(buf)
    } else {
        let mut f = File::open(path)?;
        let mut buf = String::new();
        f.read_to_string(&mut buf)?;
        Ok(buf)
    }
}
