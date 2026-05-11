//! Port of [`pappy/pappy/ReadGrammar.hs`](../../../pappy/pappy/ReadGrammar.hs) — packrat replaced
//! with a cursor parser over the full source (same recognition order).

use super::ast::*;
use super::error::PappycError;

const KEYWORDS: &[&str] = &["parser", "top", "import", "token"];

pub fn parse_grammar(file: &str, source: &str) -> Result<Grammar, PappycError> {
    let mut c = Cursor::new(file, source);
    c.skip_ws();
    c.expect_keyword("parser")?;
    let name = c.parse_identifier()?;
    let exports = if c.try_char('(') {
        let ex = c.parse_export_list()?;
        c.expect_char(')')?;
        Some(ex)
    } else {
        None
    };
    c.expect_char(':')?;
    let lp1 = c.line_pragma();

    let c1 = if c.try_char('{') {
        let inner = c.parse_brace_contents()?;
        c.expect_char('}')?;
        c.skip_ws();
        inner
    } else {
        String::new()
    };

    let imports = if c.try_keyword("import") {
        let mut v = vec![c.parse_string_lit()?];
        while c.try_char(',') {
            v.push(c.parse_string_lit()?);
        }
        v
    } else {
        vec![]
    };

    let token = if c.try_keyword("token") {
        Some(c.parse_identifier()?)
    } else {
        None
    };

    c.expect_keyword("top")?;
    let mut tops = vec![c.parse_identifier()?];
    while c.try_char(',') {
        tops.push(c.parse_identifier()?);
    }

    let mut ns = Vec::new();
    loop {
        c.skip_ws();
        if c.is_eof() {
            break;
        }
        if c.peek() == Some('{') {
            break;
        }
        // start of nonterminal: identifier '::'
        if !c.starts_ident() {
            return Err(c.err("expected nonterminal or final `{` block"));
        }
        let la = c.lookahead_nonterminal();
        if la {
            ns.push(c.parse_nonterminal()?);
        } else {
            break;
        }
    }

    let lp2 = c.line_pragma();
    let c2 = if c.try_char('{') {
        let inner = c.parse_brace_contents()?;
        c.expect_char('}')?;
        c.skip_ws();
        inner
    } else {
        String::new()
    };

    c.skip_ws();
    if !c.is_eof() {
        return Err(c.err("trailing input after grammar"));
    }

    let code = format!("{lp1}{c1}\n{lp2}{c2}\n");
    Ok(Grammar {
        grammar_name: name,
        grammar_raw_code: code,
        grammar_token: token,
        grammar_imports: imports,
        grammar_tops: tops,
        grammar_exports: exports,
        grammar_nonterminals: ns,
    })
}

struct Cursor<'a> {
    file: &'a str,
    s: &'a str,
    i: usize,
}

impl<'a> Cursor<'a> {
    fn new(file: &'a str, s: &'a str) -> Self {
        Cursor { file, s, i: 0 }
    }

    fn err(&self, msg: impl Into<String>) -> PappycError {
        PappycError::Parse {
            file: self.file.to_string(),
            message: msg.into(),
            offset: self.i,
        }
    }

    fn is_eof(&self) -> bool {
        self.i >= self.s.len()
    }

    fn peek(&self) -> Option<char> {
        self.s[self.i..].chars().next()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.i += c.len_utf8();
        Some(c)
    }

    fn line_col(&self) -> (usize, usize) {
        let mut line = 1usize;
        let mut col = 1usize;
        for ch in self.s[..self.i].chars() {
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (line, col)
    }

    fn line_pragma(&self) -> String {
        let (l, _) = self.line_col();
        format!("\n// pappy line {l} {:?}\n", self.file)
    }

    fn skip_ws(&mut self) {
        loop {
            while let Some(c) = self.peek() {
                if c.is_whitespace() {
                    self.bump();
                } else {
                    break;
                }
            }
            if self.s[self.i..].starts_with("--") {
                while let Some(c) = self.peek() {
                    self.bump();
                    if c == '\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }

    fn try_char(&mut self, ch: char) -> bool {
        self.skip_ws();
        if self.peek() == Some(ch) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect_char(&mut self, ch: char) -> Result<(), PappycError> {
        self.skip_ws();
        if self.peek() == Some(ch) {
            self.bump();
            Ok(())
        } else {
            Err(self.err(format!("expected `{ch}`")))
        }
    }

    fn expect_keyword(&mut self, kw: &str) -> Result<(), PappycError> {
        self.skip_ws();
        let rest = &self.s[self.i..];
        if rest.starts_with(kw) {
            let next = rest[kw.len()..].chars().next();
            if next.map(|c| !is_ident_cont(c)).unwrap_or(true) {
                self.i += kw.len();
                return Ok(());
            }
        }
        Err(self.err(format!("expected keyword `{kw}`")))
    }

    fn try_keyword(&mut self, kw: &str) -> bool {
        self.skip_ws();
        let rest = &self.s[self.i..];
        if rest.starts_with(kw) {
            let next = rest[kw.len()..].chars().next();
            if next.map(|c| !is_ident_cont(c)).unwrap_or(true) {
                self.i += kw.len();
                return true;
            }
        }
        false
    }

    fn starts_ident(&self) -> bool {
        self.peek().map(is_ident_start).unwrap_or(false)
    }

    /// Heuristic: next token looks like `ident ::`
    fn lookahead_nonterminal(&mut self) -> bool {
        let save = self.i;
        self.skip_ws();
        let ok = self.parse_identifier().is_ok();
        self.skip_ws();
        let ok2 = ok && self.peek() == Some(':') && self.s[self.i + 1..].starts_with(':');
        self.i = save;
        ok2
    }

    fn parse_identifier(&mut self) -> Result<String, PappycError> {
        self.skip_ws();
        let start = self.i;
        let c = self.peek().ok_or_else(|| self.err("unexpected EOF in identifier"))?;
        if !is_ident_start(c) {
            return Err(self.err("expected identifier"));
        }
        self.bump();
        while let Some(c) = self.peek() {
            if is_ident_cont(c) {
                self.bump();
            } else {
                break;
            }
        }
        let w = &self.s[start..self.i];
        if KEYWORDS.contains(&w) {
            return Err(self.err(format!("unexpected keyword `{w}`")));
        }
        Ok(w.to_string())
    }

    fn parse_word(&mut self) -> Result<String, PappycError> {
        self.parse_identifier()
    }

    fn parse_export_list(&mut self) -> Result<Vec<String>, PappycError> {
        let mut out = vec![];
        loop {
            let x = self.parse_identifier()?;
            let y = if self.try_char('(') {
                let inner = self.parse_inside_parens()?;
                self.expect_char(')')?;
                format!("({inner})")
            } else {
                String::new()
            };
            out.push(x + &y);
            if !self.try_char(',') {
                break;
            }
        }
        Ok(out)
    }

    fn parse_inside_parens(&mut self) -> Result<String, PappycError> {
        let start = self.i;
        let mut depth = 0usize;
        loop {
            let c = self.peek().ok_or_else(|| self.err("unclosed `(` in export"))?;
            if c == '(' {
                depth += 1;
            } else if c == ')' {
                if depth == 0 {
                    break;
                }
                depth -= 1;
            }
            self.bump();
        }
        Ok(self.s[start..self.i].to_string())
    }

    fn parse_string_lit(&mut self) -> Result<String, PappycError> {
        self.skip_ws();
        self.expect_char('"')?;
        let mut out = String::new();
        loop {
            let c = self.bump().ok_or_else(|| self.err("unclosed string"))?;
            if c == '"' {
                break;
            }
            if c == '\\' {
                let e = self.bump().ok_or_else(|| self.err("unclosed escape"))?;
                out.push(match e {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'v' => '\u{000b}',
                    'f' => '\u{000c}',
                    '\\' => '\\',
                    '"' => '"',
                    _ => e,
                });
            } else {
                out.push(c);
            }
        }
        Ok(out)
    }

    fn parse_char_lit(&mut self) -> Result<char, PappycError> {
        self.skip_ws();
        self.expect_char('\'')?;
        let c = quoted_char(self)?;
        self.expect_char('\'')?;
        self.skip_ws();
        Ok(c)
    }

    fn parse_brace_contents(&mut self) -> Result<String, PappycError> {
        // Caller has consumed the opening `{`. Match Haskell `codeChars` + outer `char '}'`:
        // inner text does not include the closing `}`; caller calls `expect_char('}')`.
        let mut depth = 1usize;
        let mut out = String::new();
        loop {
            let c = self.peek().ok_or_else(|| self.err("unclosed `{` in code block"))?;
            if c == '{' {
                depth += 1;
                self.bump();
                out.push('{');
            } else if c == '}' {
                if depth == 1 {
                    break;
                }
                depth -= 1;
                self.bump();
                out.push('}');
            } else if c == '\'' {
                // Haskell: `'` can start a char literal `'x'` or be a trailing prime on an
                // identifier (`sb'`). If the next byte cannot start a char body, treat as prime.
                self.bump();
                match self.peek() {
                    Some('}' | ')' | ',' | ';' | ']' | ' ' | '\t' | '\n' | '\r') => {
                        out.push('\'');
                    }
                    Some(_) => {
                        out.push('\'');
                        let inner = self.parse_code_sq()?;
                        out.push_str(&inner);
                        self.expect_char('\'')?;
                        out.push('\'');
                    }
                    None => return Err(self.err("unclosed `{` in code block")),
                }
            } else if c == '"' {
                self.bump();
                out.push('"');
                let inner = self.parse_code_dq()?;
                out.push_str(&inner);
                self.expect_char('"')?;
                out.push('"');
            } else if self.s[self.i..].starts_with("--") {
                while let Some(ch) = self.peek() {
                    out.push(ch);
                    self.bump();
                    if ch == '\n' {
                        break;
                    }
                }
            } else {
                out.push(c);
                self.bump();
            }
        }
        Ok(out)
    }

    fn parse_code_sq(&mut self) -> Result<String, PappycError> {
        let mut s = String::new();
        loop {
            match self.peek() {
                None => return Err(self.err("unclosed char in code")),
                Some('\'') | Some('\r' | '\n') => break,
                Some('\\') => {
                    self.bump();
                    let e = self.bump().ok_or_else(|| self.err("bad escape"))?;
                    s.push('\\');
                    s.push(e);
                }
                Some(c) => {
                    self.bump();
                    s.push(c);
                }
            }
        }
        Ok(s)
    }

    fn parse_code_dq(&mut self) -> Result<String, PappycError> {
        let mut s = String::new();
        loop {
            match self.peek() {
                None => return Err(self.err("unclosed string in code")),
                Some('"') | Some('\r' | '\n') => break,
                Some('\\') => {
                    self.bump();
                    let e = self.bump().ok_or_else(|| self.err("bad escape"))?;
                    s.push('\\');
                    s.push(e);
                }
                Some(c) => {
                    self.bump();
                    s.push(c);
                }
            }
        }
        Ok(s)
    }

    fn parse_sym(&mut self, sym: &str) -> Result<(), PappycError> {
        self.skip_ws();
        if self.s[self.i..].starts_with(sym) {
            self.i += sym.len();
            Ok(())
        } else {
            Err(self.err(format!("expected symbol `{sym}`")))
        }
    }

    fn parse_nonterminal(&mut self) -> Result<Nonterminal, PappycError> {
        let n = self.parse_identifier()?;
        self.parse_sym("::")?;
        let t = if self.try_sym("()") {
            "()".to_string()
        } else if self.peek() == Some('{') {
            self.bump();
            let inner = self.parse_brace_contents()?;
            self.expect_char('}')?;
            format!("{{{inner}}}")
        } else {
            self.parse_identifier()?
        };
        self.skip_ws();
        self.parse_sym("=")?;
        let r = self.parse_alt_rule()?;
        let r = if t == "()" {
            discard_rule(r)
        } else {
            r
        };
        Ok((n, t, r))
    }

    fn try_sym(&mut self, sym: &str) -> bool {
        self.skip_ws();
        if sym == "?" && self.s[self.i..].starts_with("?!") {
            return false;
        }
        if self.s[self.i..].starts_with(sym) {
            self.i += sym.len();
            true
        } else {
            false
        }
    }

    fn parse_alt_rule(&mut self) -> Result<Rule, PappycError> {
        let mut alts = vec![self.parse_seq_rule()?];
        while self.try_sym("/") {
            alts.push(self.parse_seq_rule()?);
        }
        let sl = if self.try_sym("?!") {
            Some(self.parse_string_lit()?)
        } else {
            None
        };
        let mut rule = if alts.len() == 1 {
            alts.pop().unwrap()
        } else {
            Rule::RuleAlt(alts)
        };
        if let Some(msg) = sl {
            rule = Rule::RuleError(Box::new(rule), msg);
        }
        Ok(rule)
    }

    fn parse_seq_rule(&mut self) -> Result<Rule, PappycError> {
        let save = self.i;
        let mut ms = Vec::new();
        loop {
            self.skip_ws();
            if self.s[self.i..].starts_with("->") {
                break;
            }
            if self.peek() == Some('/') || self.s[self.i..].starts_with("?!") {
                break;
            }
            let ck = self.i;
            match self.parse_seq_match() {
                Ok(m) => ms.push(m),
                Err(_) => {
                    self.i = ck;
                    break;
                }
            }
        }
        self.skip_ws();
        if self.s[self.i..].starts_with("->") {
            self.parse_sym("->")?;
            self.skip_ws();
            if self.try_sym("()") {
                return Ok(Rule::RuleSeq(
                    ms,
                    Producer::ProdCode("()".into()),
                ));
            }
            if self.peek() == Some('{') {
                let code = self.parse_raw_code()?;
                return Ok(Rule::RuleSeq(ms, Producer::ProdCode(code)));
            }
            let id = self.parse_identifier()?;
            return Ok(Rule::RuleSeq(ms, Producer::ProdName(id)));
        }
        self.i = save;
        self.parse_unary_rule()
    }

    fn parse_seq_match(&mut self) -> Result<Match, PappycError> {
        self.skip_ws();
        if self.try_sym("&") {
            if self.peek() == Some('{') {
                self.bump();
                let p = self.parse_brace_contents()?;
                self.expect_char('}')?;
                return Ok(Match::MatchPred(p));
            }
            let r = self.parse_unary_rule()?;
            return Ok(Match::MatchAnd(r));
        }
        if self.try_sym("!") {
            let r = self.parse_unary_rule()?;
            return Ok(Match::MatchNot(r));
        }
        let save = self.i;
        if self.starts_ident() {
            let id = self.parse_identifier()?;
            self.skip_ws();
            if self.peek() == Some(':') && self.s[self.i + 1..].starts_with(':') {
                self.i = save;
            } else if self.peek() == Some(':') {
                self.bump();
                let r = self.parse_unary_rule()?;
                return Ok(Match::MatchName(r, id));
            } else {
                self.i = save;
            }
        }
        if self.peek() == Some('{') {
            self.bump();
            let p = self.parse_brace_contents()?;
            self.expect_char('}')?;
            self.expect_char(':')?;
            let r = self.parse_unary_rule()?;
            return Ok(Match::MatchPat(r, p));
        }
        if self.peek() == Some('\'') {
            let c = self.parse_char_lit()?;
            self.expect_char(':')?;
            let r = self.parse_unary_rule()?;
            return Ok(Match::MatchString(r, c.to_string()));
        }
        if self.peek() == Some('"') {
            let save = self.i;
            let s = self.parse_string_lit()?;
            self.skip_ws();
            // `"foo":rule` (named string fragment). Plain `"foo"` must fall through to
            // `parse_unary_rule` → `RuleString` (e.g. `"%%%END%%%"` in Lojban.pappy).
            if self.peek() == Some(':') && !self.s[self.i + 1..].starts_with(':') {
                self.bump(); // the single `:`
                let r = self.parse_unary_rule()?;
                return Ok(Match::MatchString(r, s));
            }
            self.i = save;
        }
        let r = self.parse_unary_rule()?;
        Ok(Match::MatchAnon(r))
    }

    fn parse_raw_code(&mut self) -> Result<String, PappycError> {
        self.skip_ws();
        if self.peek() == Some('{') {
            self.bump();
            let inner = self.parse_brace_contents()?;
            self.expect_char('}')?;
            Ok(inner)
        } else {
            Err(self.err("expected `{` for raw code"))
        }
    }

    fn parse_unary_rule(&mut self) -> Result<Rule, PappycError> {
        let mut r = self.parse_prim_rule()?;
        loop {
            self.skip_ws();
            if self.try_sym("?") {
                r = Rule::RuleOpt(Box::new(r));
            } else if self.try_sym("+") {
                r = Rule::RulePlus(Box::new(r));
            } else if self.try_sym("*") {
                r = Rule::RuleStar(Box::new(r));
            } else {
                break;
            }
        }
        Ok(r)
    }

    fn parse_prim_rule(&mut self) -> Result<Rule, PappycError> {
        self.skip_ws();
        if self.try_char('(') {
            // inner alt or discard tuple
            let inner = self.parse_alt_rule()?;
            self.expect_char(')')?;
            return Ok(inner);
        }
        if self.starts_ident() {
            let id = self.parse_identifier()?;
            if id == "Pos" {
                return Ok(Rule::RulePos);
            }
            return Ok(Rule::RulePrim(id));
        }
        if self.peek() == Some('\'') {
            let c = self.parse_char_lit()?;
            return Ok(Rule::RuleChar(c));
        }
        if self.peek() == Some('"') {
            let s = self.parse_string_lit()?;
            return Ok(Rule::RuleString(s));
        }
        Err(self.err("expected primitive rule"))
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_cont(c: char) -> bool {
    is_ident_start(c) || c.is_ascii_digit() || c == '\''
}

fn quoted_char(c: &mut Cursor<'_>) -> Result<char, PappycError> {
    let ch = c.bump().ok_or_else(|| c.err("char literal"))?;
    if ch == '\\' {
        let e = c.bump().ok_or_else(|| c.err("escape"))?;
        Ok(match e {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\u{000b}',
            'f' => '\u{000c}',
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            _ => e,
        })
    } else {
        Ok(ch)
    }
}

fn discard_rule(r: Rule) -> Rule {
    fn f(r: Rule) -> Rule {
        match r {
            Rule::RuleSeq(ms, _) => Rule::RuleSeq(ms, Producer::ProdCode("()".into())),
            Rule::RuleAlt(mut a) if a.len() == 1 => f(a.pop().unwrap()),
            Rule::RuleAlt(a) => Rule::RuleAlt(a.into_iter().map(f).collect()),
            r => Rule::RuleSeq(vec![Match::MatchAnon(r)], Producer::ProdCode("()".into())),
        }
    }
    f(r)
}
