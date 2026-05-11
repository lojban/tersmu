/// Character position (file, line, column) — port of `Pappy.Pos`.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
    pub file: String,
    pub line: i32,
    pub col: i32,
}

pub fn next_pos(p: &Pos, c: char) -> Pos {
    if c == '\n' {
        Pos {
            file: p.file.clone(),
            line: p.line + 1,
            col: 1,
        }
    } else if c == '\t' {
        let col = p.col;
        let tab = ((col + 8 - 1) / 8) * 8 + 1;
        Pos {
            file: p.file.clone(),
            line: p.line,
            col: tab,
        }
    } else {
        Pos {
            file: p.file.clone(),
            line: p.line,
            col: p.col + 1,
        }
    }
}
