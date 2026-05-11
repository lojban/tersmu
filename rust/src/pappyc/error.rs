use std::fmt;

#[derive(Debug)]
pub enum PappycError {
    Io { path: String, message: String },
    Parse { file: String, message: String, offset: usize },
    Reduce(String),
}

impl fmt::Display for PappycError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PappycError::Io { path, message } => {
                write!(f, "cannot read {path}: {message}")
            }
            PappycError::Parse {
                file,
                message,
                offset,
            } => write!(f, "{file}: parse error at byte {offset}: {message}"),
            PappycError::Reduce(msg) => f.write_str(msg),
        }
    }
}

impl std::error::Error for PappycError {}
